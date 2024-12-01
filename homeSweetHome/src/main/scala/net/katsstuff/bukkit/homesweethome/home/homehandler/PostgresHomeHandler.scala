package net.katsstuff.bukkit.homesweethome.home.homehandler

import java.time.OffsetDateTime
import java.util.UUID
import javax.sql.DataSource

import scala.collection.mutable
import scala.concurrent.duration.*
import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.CollectionConverters.*

import cats.data.NonEmptyList
import cats.effect.{IO, Resource}
import cats.syntax.all.*
import dataprism.KMacros
import dataprism.skunk.platform.PostgresSkunkPlatform.Api.*
import dataprism.skunk.sql.SkunkTypes.*
import dataprism.sql.*
import io.circe.*
import io.circe.syntax.*
import net.katsstuff.bukkit.homesweethome.home.Home
import net.katsstuff.bukkit.homesweethome.home.homehandler.PostgresHomeHandler.{InviteK, OnlinePlayerGlobalK, RequestK}
import net.katsstuff.bukkit.homesweethome.home.storage.HomeStorage
import net.katsstuff.bukkit.homesweethome.lib.LibPerm
import net.katsstuff.bukkit.homesweethome.{HSHConfig, HomePlugin, NestedMap}
import net.katsstuff.bukkit.katlib.db.PostgresCached
import net.katsstuff.bukkit.katlib.util.FutureOrNow
import net.katsstuff.bukkit.katlib.{GlobalPlayer, ScalaPlugin}
import net.milkbowl.vault.chat.Chat
import org.bukkit.event.player.{PlayerJoinEvent, PlayerQuitEvent}
import org.bukkit.event.{EventHandler, Listener}
import org.bukkit.{Bukkit, OfflinePlayer, World}
import skunk.{Codec, Session}

class PostgresHomeHandler(storage: HomeStorage, sessionPool: Resource[IO, Session[IO]])(
    using db: Db[Future, Codec],
    sc: HomePlugin,
    ec: ExecutionContext,
    hshConfig: HSHConfig
) extends HomeHandler(storage)
    with AutoCloseable
    with Listener:

  given SqlOrdered[OffsetDateTime] = SqlOrdered.defaultInstance[OffsetDateTime]

  private def chat: Chat = Bukkit.getServicesManager.load(classOf[Chat])

  private val globalOnlinePlayersCached =
    PostgresCached.postgresNotify[Seq[GlobalPlayer]](
      () => FutureOrNow.fromFuture(Select(Query.from(OnlinePlayerGlobalK.table)).run.map(_.map(_.asGlobalPlayer))),
      60.seconds,
      "HomeSweetHome.GlobalPlayersChange",
      sessionPool,
      onCreate = Some((old, json) =>
        json.hcursor
          .get[UUID]("uuid")
          .flatMap { uuid =>
            Option(Bukkit.getPlayer(uuid)).fold(
              json.hcursor.get[String]("name").map(GlobalPlayer.OnOtherServer(_, uuid))
            )(p => Right(GlobalPlayer.OnThisServer(p)))
          }
          .map(p => old.appended(p).distinctBy(_.uuid))
      ),
      onDelete = Some((old, json) => json.hcursor.get[UUID]("uuid").map(uuid => old.filterNot(_.uuid == uuid)))
    )

  private def seqToNestedMap(seq: Seq[(UUID, UUID, Decoder.Result[Home])]): NestedMap[UUID, UUID, Home] =
    NestedMap(
      seq
        .groupBy(_._1)
        .map { t =>
          t._1 -> t._2
            .groupMapReduce(_._2)(_._3)((a, _) => a)
            .flatMap(t => t._2.toOption.map(t._1 -> _))
            .to(mutable.Map)
        }
        .to(mutable.Map),
      () => mutable.Map()
    )

  private val requests = PostgresCached.postgresNotify[NestedMap[UUID, UUID, Home]](
    () => {
      Delete
        .from(RequestK.table)
        .where(r => r.expires >= OffsetDateTime.now().as(timestamptz))
        .run

      FutureOrNow.fromFuture(
        Select(
          Query.from(RequestK.table).where(_.expires < OffsetDateTime.now().as(timestamptz))
        ).run.map { res =>
          seqToNestedMap(res.map(i => (i.requester, i.homeOwner, i.home.as[Home])))
        }
      )
    },
    60.seconds,
    "HomeSweetHome.RequestsChange",
    sessionPool,
    onCreate = Some((old, json) =>
      for
        requester <- json.hcursor.get[UUID]("requester")
        homeOwner <- json.hcursor.get[UUID]("homeOwner")
        home      <- json.hcursor.get[Home]("home")
      yield { old.update(requester, homeOwner, home); old }
    ),
    onDelete = Some((old, json) =>
      for
        requester <- json.hcursor.get[UUID]("requester")
        homeOwner <- json.hcursor.get[UUID]("homeOwner")
      yield { old.remove(requester, homeOwner); old }
    )
  )

  private val invites = PostgresCached.postgresNotify[NestedMap[UUID, UUID, Home]](
    () => {
      Delete
        .from(InviteK.table)
        .where(r => r.expires >= OffsetDateTime.now().as(timestamptz))
        .run

      FutureOrNow.fromFuture(
        Select(
          Query.from(InviteK.table).where(_.expires < OffsetDateTime.now().as(timestamptz))
        ).run.map { res =>
          seqToNestedMap(res.map(i => (i.target, i.homeOwner, i.home.as[Home])))
        }
      )
    },
    60.seconds,
    "HomeSweetHome.InvitesChange",
    sessionPool,
    onCreate = Some((old, json) =>
      for
        requester <- json.hcursor.get[UUID]("target")
        homeOwner <- json.hcursor.get[UUID]("homeOwner")
        home      <- json.hcursor.get[Home]("home")
      yield { old.update(requester, homeOwner, home); old }
    ),
    onDelete = Some((old, json) =>
      for
        requester <- json.hcursor.get[UUID]("target")
        homeOwner <- json.hcursor.get[UUID]("homeOwner")
      yield { old.remove(requester, homeOwner); old }
    )
  )

  override def reload(): Future[Unit] =
    Delete
      .from(OnlinePlayerGlobalK.table)
      .where(p => p.server === hshConfig.serverName.as(text))
      .run
      .flatMap { _ =>
        Bukkit.getOnlinePlayers.asScala
          .map(p => OnlinePlayerGlobalK[perspective.Id](p.getUniqueId, p.getName, hshConfig.serverName))
          .toList match
          case head :: tail => Insert.into(OnlinePlayerGlobalK.table).values(head, tail*).run
          case Nil          => Future.unit
      }
      .flatMap { _ =>
        storage.reloadHomeData()
      }

  override def close(): Unit =
    globalOnlinePlayersCached.close()
    requests.close()
    invites.close()

  override def globalOnlinePlayers: Seq[GlobalPlayer] = globalOnlinePlayersCached.get

  override def addRequest(requester: UUID, homeOwner: UUID, home: Home): FutureOrNow[Unit] =
    FutureOrNow.fromFuture(
      Insert
        .into(RequestK.table)
        .values(
          RequestK(
            requester,
            homeOwner,
            home.asJson,
            expires = OffsetDateTime.now().plusSeconds(hshConfig.home.timeout)
          )
        )
        .onConflictUpdate(t => NonEmptyList.of(t.requester, t.homeOwner))
        .run
        .void
    )

  override def removeRequest(requester: UUID, homeOwner: UUID): FutureOrNow[Unit] =
    FutureOrNow.fromFuture(
      Delete
        .from(RequestK.table)
        .where(t => t.requester === requester.as(uuid) && t.homeOwner === homeOwner.as(uuid))
        .run
        .void
    )

  override def getRequest(requester: UUID, homeOwner: UUID): Option[Home] = requests.get.get(requester, homeOwner)

  override def getAllRequestersForPlayer(homeOwner: UUID): Seq[UUID] = requests.get.toNormalMap
    .filter { case (_, inner) =>
      inner.contains(homeOwner)
    }
    .keys
    .toSeq

  override def addInvite(target: UUID, homeOwner: UUID, home: Home): FutureOrNow[Unit] =
    FutureOrNow.fromFuture(
      Insert
        .into(InviteK.table)
        .values(
          InviteK(target, homeOwner, home.asJson, expires = OffsetDateTime.now().plusSeconds(hshConfig.home.timeout))
        )
        .onConflictUpdate(t => NonEmptyList.of(t.target, t.homeOwner))
        .run
        .void
    )

  override def removeInvite(player: UUID, homeOwner: UUID): FutureOrNow[Unit] =
    FutureOrNow.fromFuture(
      Delete
        .from(InviteK.table)
        .where(t => t.target === player.as(uuid) && t.homeOwner === homeOwner.as(uuid))
        .run
        .void
    )

  override def isInvited(target: UUID, homeOwner: UUID, home: Home): Boolean =
    invites.get.get(target, homeOwner).contains(home)

  /** The amount of homes a player can have */
  def getHomeLimit(world: World, player: OfflinePlayer): Int =
    chat.getPlayerInfoInteger(world.getName, player, LibPerm.HomeLimitOption, hshConfig.home.homeLimit)

  /** The amount of residents a home for a player can have */
  def getResidentLimit(world: World, player: OfflinePlayer): Int =
    chat.getPlayerInfoInteger(world.getName, player, LibPerm.ResidentLimitOption, hshConfig.home.residentLimit)

  @EventHandler(ignoreCancelled = true)
  def onPlayerJoin(event: PlayerJoinEvent): Unit =
    val player = event.getPlayer
    Insert
      .into(OnlinePlayerGlobalK.table)
      .values(OnlinePlayerGlobalK(player.getUniqueId, player.getName, hshConfig.serverName))
      .onConflictUpdate(t => NonEmptyList.one(t.uuid))
      .run

  @EventHandler(ignoreCancelled = true)
  def onPlayerLeave(event: PlayerQuitEvent): Unit =
    val player = event.getPlayer
    Delete
      .from(OnlinePlayerGlobalK.table)
      .where { p =>
        p.uuid === player.getUniqueId.as(uuid)
        && p.name === player.getName.as(text)
        && p.server === hshConfig.serverName.as(text)
      }
      .run

object PostgresHomeHandler:

  case class OnlinePlayerGlobalK[F[_]](
      uuid: F[UUID],
      name: F[String],
      server: F[String]
  )
  object OnlinePlayerGlobalK:
    given instance[F[_]](using uuid: F[UUID], string: F[String]): OnlinePlayerGlobalK[F] =
      OnlinePlayerGlobalK(uuid, string, string)

    given typeclass: KMacros.RepresentableTraverseKC[OnlinePlayerGlobalK] =
      KMacros.deriveRepresentableTraverseKC[OnlinePlayerGlobalK]

    val table: Table[Codec, OnlinePlayerGlobalK] = Table(
      "online_players",
      OnlinePlayerGlobalK(
        Column("uuid", uuid),
        Column("name", text),
        Column("server", text)
      )
    )

    extension (online: OnlinePlayerGlobalK[perspective.Id])
      def asGlobalPlayer: GlobalPlayer = Option(Bukkit.getPlayer(online.uuid))
        .fold(GlobalPlayer.OnOtherServer(online.name, online.uuid))(GlobalPlayer.OnThisServer(_))

  case class RequestK[F[_]](
      requester: F[UUID],
      homeOwner: F[UUID],
      home: F[Json],
      expires: F[OffsetDateTime]
  )
  object RequestK:
    given instance[F[_]](using uuid: F[UUID], json: F[Json], time: F[OffsetDateTime]): RequestK[F] =
      RequestK(uuid, uuid, json, time)
    given typeclass: KMacros.RepresentableTraverseKC[RequestK] = KMacros.deriveRepresentableTraverseKC[RequestK]

    val table: Table[Codec, RequestK] = Table(
      "requests",
      RequestK(
        Column("requester", uuid),
        Column("home_owner", uuid),
        Column("home", skunk.circe.codec.json.jsonb.wrap),
        Column("expires", timestamptz)
      )
    )

  case class InviteK[F[_]](
      target: F[UUID],
      homeOwner: F[UUID],
      home: F[Json],
      expires: F[OffsetDateTime]
  )
  object InviteK:
    given instance[F[_]](using uuid: F[UUID], json: F[Json], time: F[OffsetDateTime]): InviteK[F] =
      InviteK(uuid, uuid, json, time)
    given typeclass: KMacros.RepresentableTraverseKC[InviteK] = KMacros.deriveRepresentableTraverseKC[InviteK]

    val table: Table[Codec, InviteK] = Table(
      "invites",
      InviteK(
        Column("target", uuid),
        Column("home_owner", uuid),
        Column("home", skunk.circe.codec.json.jsonb.wrap),
        Column("expires", timestamptz)
      )
    )
