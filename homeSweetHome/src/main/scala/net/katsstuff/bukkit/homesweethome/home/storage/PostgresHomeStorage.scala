package net.katsstuff.bukkit.homesweethome.home.storage

import java.util.UUID
import scala.concurrent.duration.*
import scala.concurrent.{ExecutionContext, Future}
import cats.data.NonEmptyList
import dataprism.skunk.platform.PostgresSkunkPlatform
import dataprism.skunk.platform.PostgresSkunkPlatform.Api.*
import dataprism.skunk.sql.SkunkTypes.*
import dataprism.sql.*
import net.katsstuff.bukkit.homesweethome.home.{Home, HomeK, Resident, ResidentK}
import net.katsstuff.bukkit.homesweethome.{HSHConfig, HomePlugin}
import net.katsstuff.bukkit.katlib.util.{CachedRemoteData, FutureOrNow}
import org.bukkit.Location
import org.bukkit.event.EventHandler
import org.bukkit.event.player.{PlayerJoinEvent, PlayerQuitEvent}
import perspective.derivation.ProductK
import skunk.Codec

class PostgresHomeStorage(
    using db: Db[Future, Codec],
    plugin: HomePlugin,
    ec: ExecutionContext,
    hshConfig: HSHConfig
) extends CachedHomeStorage {

  override def fetchAllHomesForPlayer(uuidV: UUID): Future[Map[String, Home]] =
    Select(Query.from(HomeK.table).filter(_.owner === uuidV.as(uuid))).run
      .map(_.groupMapReduce(_.name)(identity)((_, b) => b))

  override def fetchSpecificHome(uuidV: UUID, name: String): Future[Option[Home]] =
    Select(
      Query
        .from(HomeK.table)
        .filter(home => home.owner === uuidV.as(uuid) && home.name === name.as(text))
    ).runMaybeOne

  override def fetchHomeCount(uuidV: UUID): Future[Int] =
    Select(Query.of(Query.from(HomeK.table).filter(_.owner === uuidV.as(uuid)).size)).runOne[Future].map(_.toInt)

  override def fetchHomeExist(uuidV: UUID, name: String): Future[Boolean] =
    Select(
      Query.of(
        Query
          .from(HomeK.table)
          .filter(home => home.owner === uuidV.as(uuid) && home.name === name.as(text))
          .nonEmpty
      )
    ).runOne

  override def saveHome(home: Home): Future[Unit] =
    Insert
      .into(HomeK.table)
      .values(home)
      .onConflictUpdate(t => NonEmptyList.of(t.owner, t.name))
      .run
      .map(_ => ())

  override def deleteSavedHome(uuidV: UUID, name: String): Future[Unit] =
    Delete
      .from(HomeK.table)
      .where(home => home.owner === uuidV.as(uuid) && home.name === name.as(text))
      .run
      .map(_ => ())

  override def searchHomes(
      location: Location,
      radius: Option[Double],
      world: Option[UUID],
      owner: Option[UUID],
      drop: Int,
      take: Int
  ): FutureOrNow[Seq[Home]] =
    def square2(v: DbValue[Double]): DbValue[Double] = v * v

    val distanceToPlayerSq2: HomeK[DbValue] => DbValue[Double] = h =>
      square2(h.x - location.getX.as(float8)) +
        square2(h.y - location.getY.as(float8)) +
        square2(h.z - location.getZ.as(float8))

    val filters2: Seq[Option[Query[HomeK] => Query[HomeK]]] = Seq(
      radius.map { r =>
        _.filter { h =>
          distanceToPlayerSq2(h) < (r * r).as(float8)
        }
      },
      world.map(w => _.filter(_.worldUuid === w.as(uuid))),
      owner.map(o => _.filter(_.owner === o.as(uuid)))
    )

    FutureOrNow.fromFuture(
      Select(
        filters2.flatten
          .foldLeft(Query.from(HomeK.table))((q, f) => f(q))
          .orderBy(h => distanceToPlayerSq2(h).asc)
          .drop(drop)
          .take(take)
      ).run
    )

  extension [A](v: Many[A])
    private def arrayAgg: DbValue[Seq[A]] =
      def arrayType(elemType: Type[A])(using extraArrayTypeArgs: DummyImplicit): Type[Seq[A]] =
        NullabilityTypeChoice
          .notNullByDefault(
            Codec
              .array[A](
                a => elemType.codec.encode(a).head.get,
                s => elemType.codec.decode(0, List(Some(s))).left.map(_.message),
                elemType.codec.types.head
              )
              .imap(arr => Seq.tabulate(arr.size)(arr.get(_).get))(seq => skunk.data.Arr(seq*)),
            _.opt
          )
          .notNull

      DbValue.function("array_agg", arrayType(v.unsafeAsDbValue.tpe))(v.unsafeAsDbValue)

  override def fetchAllResidentsForPlayer(homeOwner: UUID): Future[Map[String, Set[UUID]]] =
    Select(
      Query
        .from(ResidentK.table)
        .filter(resident => resident.owner === homeOwner.as(uuid))
        .groupMap(_.homeName)((name, r) => (name, r.resident.arrayAgg))
    ).run.map { ret =>
      ret.map(t => t._1 -> t._2.toSet).toMap
    }

  override def fetchGetHomeResidents(homeOwner: UUID, homeName: String): Future[Set[UUID]] =
    Select(
      Query
        .from(ResidentK.table)
        .filter(r => r.owner === homeOwner.as(uuid) && r.homeName === homeName.as(text))
        .map(_.resident)
    ).run.map(_.toSet)

  override def fetchIsPlayerResident(homeOwner: UUID, homeName: String, player: UUID): Future[Boolean] =
    Select(
      Query.of(
        Query
          .from(ResidentK.table)
          .filter(r => r.owner === homeOwner.as(uuid) && r.homeName === homeName.as(text))
          .nonEmpty
      )
    ).runOne

  override def saveResident(resident: Resident): Future[Unit] =
    Insert.into(ResidentK.table).values(resident).run.map(_ => ())

  override def removeSavedResident(homeOwner: UUID, homeName: String, resident: UUID): Future[Unit] =
    Delete
      .from(ResidentK.table)
      .where(r => r.owner === homeOwner.as(uuid) && r.homeName === homeName.as(text))
      .run
      .map(_ => ())

  protected val homeOwnersCache: CachedRemoteData[Set[UUID]] = CachedRemoteData.default[Set[UUID]](
    () =>
      FutureOrNow.fromFuture(
        Select(
          Query.from(HomeK.table).map(_.owner).distinct
        ).run.map(_.toSet)
      ),
    30.seconds
  )

  override def exportData(): FutureOrNow[(Seq[Home], Seq[Resident])] =
    FutureOrNow.fromFuture(
      Select(Query.from(HomeK.table)).run.zip(Select(Query.from(ResidentK.table)).run)
    )

  override def importData(homes: Seq[Home], residents: Seq[Resident]): FutureOrNow[Unit] =
    for
      _ <- FutureOrNow.fromFuture(Delete.from(HomeK.table).where(_ => DbValue.trueV).run)
      // Should be a NO-OP because of foreign keys, but just in case
      _ <- FutureOrNow.fromFuture(Delete.from(ResidentK.table).where(_ => DbValue.trueV).run)
      _ <- FutureOrNow.fromFuture(
        homes.headOption.fold(Future.successful(0))(head => Insert.into(HomeK.table).values(head, homes.tail*).run)
      )
      _ <- FutureOrNow.fromFuture(
        residents.headOption.fold(Future.successful(0))(head =>
          Insert.into(ResidentK.table).values(head, residents.tail*).run
        )
      )
    yield ()

  @EventHandler(ignoreCancelled = true) override def onPlayerJoin(event: PlayerJoinEvent): Unit =
    super.onPlayerJoin(event)

  @EventHandler(ignoreCancelled = true) override def onPlayerLeave(event: PlayerQuitEvent): Unit =
    super.onPlayerLeave(event)
}
