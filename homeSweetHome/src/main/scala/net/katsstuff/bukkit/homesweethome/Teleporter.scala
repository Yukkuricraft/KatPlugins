package net.katsstuff.bukkit.homesweethome

import java.time.OffsetDateTime
import java.util.UUID
import scala.concurrent.duration.*
import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.CollectionConverters.*
import cats.effect.{IO, Resource}
import dataprism.KMacros
import dataprism.skunk.platform.PostgresSkunkPlatform.Api.*
import dataprism.skunk.sql.SkunkTypes.*
import dataprism.sql.*
import io.circe.DecodingFailure
import net.katsstuff.bukkit.katlib.text.*
import net.katsstuff.bukkit.katlib.util.FutureOrNow
import org.bukkit.event.player.PlayerJoinEvent
import org.bukkit.event.player.PlayerTeleportEvent.TeleportCause
import org.bukkit.event.{EventHandler, Listener}
import org.bukkit.{Bukkit, Location}
import skunk.{Codec, Session}

trait Teleporter {

  def teleport(player: GlobalPlayer, destination: Location, worldUuid: UUID, server: String): Either[String, Unit]
}
object Teleporter {

  given SqlOrdered[OffsetDateTime] = SqlOrdered.defaultInstance[OffsetDateTime]

  private class SameServerTeleporter(using hshConfig: HSHConfig, plugin: HomePlugin) extends Teleporter:
    def teleportReportingError(
        player: GlobalPlayer,
        destination: Location,
        worldUuid: UUID,
        server: String
    ): Either[String, Unit] =
      val res = teleport(player, destination, worldUuid, server)
      res.swap.foreach { e =>
        player match
          case GlobalPlayer.OnThisServer(player) => player.sendMessage(t"${TextColor.Red}$e")
          case _                                 =>
      }
      res

    override def teleport(
        player: GlobalPlayer,
        destination: Location,
        worldUuid: UUID,
        server: String
    ): Either[String, Unit] =
      player match
        case GlobalPlayer.OnThisServer(player) if server == hshConfig.serverName =>
          Option(Bukkit.getWorld(worldUuid)).toRight("Teleport failed. World not found").flatMap { world =>
            val newDestination = new Location(
              world,
              destination.getX,
              destination.getY,
              destination.getZ,
              destination.getYaw,
              destination.getPitch
            )

            if !newDestination.getChunk.load() then Left("Failed to load destination for teleport")
            else
              Either
                .cond(player.teleport(newDestination, TeleportCause.COMMAND), (), "Teleport failed for unknown reason")
          }
        case _ => Left("Cross server teleportation is not enabled")

  def sameServerTeleporter(using hshConfig: HSHConfig, plugin: HomePlugin): Teleporter = new SameServerTeleporter

  private[Teleporter] case class DelayedTeleportK[F[_]](
      uuid: F[UUID],
      x: F[Double],
      y: F[Double],
      z: F[Double],
      yaw: F[Float],
      pitch: F[Float],
      worldUuid: F[UUID],
      server: F[String],
      expires: F[OffsetDateTime]
  )
  private[Teleporter] object DelayedTeleportK:
    given instance[F[_]](
        using uuid: F[UUID],
        double: F[Double],
        float: F[Float],
        string: F[String],
        time: F[OffsetDateTime]
    ): DelayedTeleportK[F] =
      DelayedTeleportK(uuid, double, double, double, float, float, uuid, string, time)

    given typeclass: KMacros.RepresentableTraverseKC[DelayedTeleportK] =
      KMacros.deriveRepresentableTraverseKC[DelayedTeleportK]

    val table: Table[skunk.Codec, DelayedTeleportK] = Table(
      "delayed_teleports",
      DelayedTeleportK(
        Column("uuid", uuid),
        Column("x", float8),
        Column("y", float8),
        Column("z", float8),
        Column("yaw", float4),
        Column("pitch", float4),
        Column("world_uuid", uuid),
        Column("server", text),
        Column("expires", timestamptz)
      )
    )

    extension (tp: DelayedTeleportK[perspective.Id])
      def toLocation: Location = new Location(null, tp.x, tp.y, tp.z, tp.yaw, tp.pitch)

  def crossServerPostgresTeleporter(pool: Resource[IO, Session[IO]])(
      using hshConfig: HSHConfig,
      homePlugin: HomePlugin,
      bungeeChannel: BungeeChannel,
      db: Db[Future, Codec],
      ec: ExecutionContext
  ): Teleporter & Listener & AutoCloseable = new CrossServerPostgresTeleporter(pool)

  class CrossServerPostgresTeleporter(sessionPool: Resource[IO, Session[IO]])(
      using hshConfig: HSHConfig,
      homePlugin: HomePlugin,
      bungeeChannel: BungeeChannel,
      db: Db[Future, Codec],
      ec: ExecutionContext
  ) extends Teleporter
      with Listener
      with AutoCloseable:
    private val sameServerHandler = new SameServerTeleporter

    private val cached = CachedRemoteData.postgresNotify[DelayedTeleportK.type](
      () =>
        FutureOrNow.now {
          Delete
            .from(DelayedTeleportK.table)
            .where(d => d.expires >= OffsetDateTime.now().as(timestamptz))
            .run

          Select(
            Query
              .from(DelayedTeleportK.table)
              .where { d =>
                d.server === hshConfig.serverName
                  .as(text) && d.expires < OffsetDateTime.now().as(timestamptz)
              }
          ).run.foreach { res =>
            res.foreach { tp =>
              Option(Bukkit.getPlayer(tp.uuid)).foreach { player =>
                sameServerHandler
                  .teleportReportingError(
                    GlobalPlayer.OnThisServer(player),
                    tp.toLocation,
                    tp.worldUuid,
                    hshConfig.serverName
                  )
                  .foreach { _ =>
                    Delete
                      .from(DelayedTeleportK.table)
                      .where(d => d.server === hshConfig.serverName.as(text) && d.uuid === player.getUniqueId.as(uuid))
                      .run
                  }
              }
            }
          }
          DelayedTeleportK
        },
      60.seconds,
      "HomeSweetHome.DelayedTeleportChange",
      sessionPool,
      onCreate = Some((old, json) =>
        if json.hcursor.get[String]("server").contains(hshConfig.serverName) then
          val h = json.hcursor

          for
            uuidV     <- h.get[UUID]("uuid")
            player    <- Option(Bukkit.getPlayer(uuidV)).toRight(DecodingFailure.apply("Player not found", h.history))
            x         <- h.get[Double]("x")
            y         <- h.get[Double]("y")
            z         <- h.get[Double]("z")
            yaw       <- h.get[Float]("pitch")
            pitch     <- h.get[Float]("pitch")
            worldUuid <- h.get[UUID]("world_uuid")
          yield
            val location = new Location(null, x, y, z, yaw, pitch)
            sameServerHandler
              .teleportReportingError(
                GlobalPlayer.OnThisServer(player),
                location,
                worldUuid,
                hshConfig.serverName
              )
              .foreach { _ =>
                Delete
                  .from(DelayedTeleportK.table)
                  .where(d => d.server === hshConfig.serverName.as(text) && d.uuid === uuidV.as(uuid))
                  .run
              }

            old
        else Right(old)
      )
    )

    override def close(): Unit = cached.close()

    @EventHandler(ignoreCancelled = true)
    def onPlayerJoin(event: PlayerJoinEvent): Unit =
      val player = event.getPlayer

      Select(
        Query
          .from(DelayedTeleportK.table)
          .where { d =>
            d.uuid === player.getUniqueId.as(uuid) &&
            d.server === hshConfig.serverName.as(text) &&
            d.expires < OffsetDateTime.now().as(timestamptz)
          }
      ).runMaybeOne[Future].foreach { res =>
        res.foreach { tp =>
          sameServerHandler.teleportReportingError(
            GlobalPlayer.OnThisServer(player),
            tp.toLocation,
            tp.worldUuid,
            hshConfig.serverName
          )
        }
      }

    private def makeDelayedTeleport(toTeleport: UUID, destination: Location, worldUuid: UUID, server: String): Unit =
      if server == hshConfig.serverName && Bukkit.getPlayer(toTeleport) != null then
        sameServerHandler.teleportReportingError(
          GlobalPlayer.OnThisServer(Bukkit.getPlayer(toTeleport)),
          destination,
          worldUuid,
          server
        )
      else
        Insert
          .into(DelayedTeleportK.table)
          .values(
            DelayedTeleportK(
              toTeleport,
              destination.getX,
              destination.getY,
              destination.getZ,
              destination.getYaw,
              destination.getPitch,
              worldUuid,
              server,
              OffsetDateTime.now().plusMinutes(1)
            )
          )
          .run

    override def teleport(
        player: GlobalPlayer,
        destination: Location,
        worldUuid: UUID,
        server: String
    ): Either[String, Unit] = player match
      case GlobalPlayer.OnThisServer(_) if hshConfig.serverName == server =>
        sameServerHandler.teleport(player, destination, worldUuid, server)

      case GlobalPlayer.OnThisServer(player) =>
        makeDelayedTeleport(player.getUniqueId, destination, worldUuid, server)
        player.sendMessage(t"${TextColor.Yellow}Preparing interdimensional travel. Hang tight.")
        bungeeChannel.sendPlayerToServer(player, server)
        Right(())

      case GlobalPlayer.OnOtherServer(playerName, uuid) =>
        Bukkit.getOnlinePlayers.asScala.headOption match
          case Some(localPlayer) =>
            player.sendMessage(localPlayer, t"${TextColor.Yellow}Preparing interdimensional travel. Hang tight.")
            makeDelayedTeleport(uuid, destination, worldUuid, server)
            bungeeChannel.sendOtherPlayerToServer(localPlayer, playerName, server)
            Right(())

          case None =>
            Left("No player online to send off player")
}
