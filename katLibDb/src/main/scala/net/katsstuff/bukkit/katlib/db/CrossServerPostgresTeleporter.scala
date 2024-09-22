package net.katsstuff.bukkit.katlib.db

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
import net.katsstuff.bukkit.katlib.{BungeeChannel, GlobalPlayer}
import net.katsstuff.bukkit.katlib.db.CrossServerPostgresTeleporter.DelayedTeleportK
import net.katsstuff.bukkit.katlib.text.*
import net.katsstuff.bukkit.katlib.util.{FutureOrNow, Teleporter}
import org.bukkit.event.player.PlayerJoinEvent
import org.bukkit.event.player.PlayerTeleportEvent.TeleportCause
import org.bukkit.event.{EventHandler, Listener}
import org.bukkit.{Bukkit, Location}
import skunk.{Codec, Session}

object CrossServerPostgresTeleporter:

  private[CrossServerPostgresTeleporter] case class DelayedTeleportK[F[_]](
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
  private[CrossServerPostgresTeleporter] object DelayedTeleportK:
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

class CrossServerPostgresTeleporter(sessionPool: Resource[IO, Session[IO]], currentServerName: String)(
    using dbPlugin: ScalaDbPlugin,
    bungeeChannel: BungeeChannel,
    db: Db[Future, Codec],
    ec: ExecutionContext
) extends Teleporter
    with Listener
    with AutoCloseable:
  private val sameServerHandler = new Teleporter.SameServerTeleporter(currentServerName)
  given SqlOrdered[OffsetDateTime] = SqlOrdered.defaultInstance[OffsetDateTime]

  private val cached = PostgresCached.postgresNotify[DelayedTeleportK.type](
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
              d.server === currentServerName
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
                  currentServerName
                )
                .foreach { _ =>
                  Delete
                    .from(DelayedTeleportK.table)
                    .where(d => d.server === currentServerName.as(text) && d.uuid === player.getUniqueId.as(uuid))
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
      if json.hcursor.get[String]("server").contains(currentServerName) then
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
              currentServerName
            )
            .foreach { _ =>
              Delete
                .from(DelayedTeleportK.table)
                .where(d => d.server === currentServerName.as(text) && d.uuid === uuidV.as(uuid))
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
          d.server === currentServerName.as(text) &&
          d.expires < OffsetDateTime.now().as(timestamptz)
        }
    ).runMaybeOne[Future].foreach { res =>
      res.foreach { tp =>
        sameServerHandler.teleportReportingError(
          GlobalPlayer.OnThisServer(player),
          tp.toLocation,
          tp.worldUuid,
          currentServerName
        )
      }
    }

  private def makeDelayedTeleport(toTeleport: UUID, destination: Location, worldUuid: UUID, server: String): Unit =
    if server == currentServerName && Bukkit.getPlayer(toTeleport) != null then
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
    case GlobalPlayer.OnThisServer(_) if currentServerName == server =>
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
