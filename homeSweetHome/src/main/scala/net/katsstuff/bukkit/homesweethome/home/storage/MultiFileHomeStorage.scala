package net.katsstuff.bukkit.homesweethome.home.storage

import java.nio.file.{Files, Path, Paths}
import java.util.UUID

import scala.concurrent.duration.*
import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.CollectionConverters.*
import scala.jdk.StreamConverters.*
import scala.util.Try

import cats.syntax.all.*
import io.circe.*
import io.circe.syntax.*
import net.katsstuff.bukkit.homesweethome.home.{Home, Resident}
import net.katsstuff.bukkit.homesweethome.{CachedRemoteData, HSHConfig, HomePlugin}
import net.katsstuff.bukkit.katlib.ScalaPlugin
import net.katsstuff.bukkit.katlib.util.FutureOrNow
import org.bukkit.Location
import org.bukkit.event.EventHandler
import org.bukkit.event.player.{PlayerJoinEvent, PlayerQuitEvent}

class MultiFileHomeStorage(storagePath: Path)(implicit plugin: HomePlugin, ec: ExecutionContext, hshConfig: HSHConfig)
    extends CachedHomeStorage
    with AutoCloseable {

  // Let's just hope we don't get synchronization issues here
  private def fetchHomeDataForPlayer(uuid: UUID): Future[(Map[String, Home], Map[String, Set[Resident]])] = Future {
    val playerPath = storagePath.resolve(s"$uuid.json")
    if Files.exists(playerPath) then
      val content = Files.readAllLines(storagePath.resolve(s"$uuid.json")).asScala.mkString("\n")
      Future.fromTry(
        parser
          .parse(content)
          .flatMap { json =>
            json.hcursor
              .get[Map[String, Home]]("homes")
              .product(json.hcursor.get[Map[String, Set[Resident]]]("residents"))
          }
          .toTry
      )
    else Future.successful((Map.empty, Map.empty))
  }.flatten

  override def fetchAllHomesForPlayer(uuid: UUID): Future[Map[String, Home]] =
    fetchHomeDataForPlayer(uuid).map(_._1)

  private def saveHomeData(
      uuid: UUID,
      modifyHomes: Map[String, Home] => Map[String, Home] = identity,
      modifyResidents: Map[String, Set[Resident]] => Map[String, Set[Resident]] = identity
  ): Future[Unit] =
    fetchHomeDataForPlayer(uuid).map(t => (modifyHomes(t._1), modifyResidents(t._2))).map { (homes, residents) =>
      if homes.isEmpty then Files.deleteIfExists(storagePath.resolve(s"$uuid.json"))
      else
        Files.write(
          storagePath.resolve(s"$uuid.json"),
          List(Json.obj("homes" := homes, "residents" := residents).noSpaces).asJava
        )
    }

  override def fetchSpecificHome(uuid: UUID, name: String): Future[Option[Home]] =
    fetchAllHomesForPlayer(uuid).map(_.get(name))

  override def fetchHomeCount(uuid: UUID): Future[Int] = fetchAllHomesForPlayer(uuid).map(_.size)

  override def fetchHomeExist(uuid: UUID, name: String): Future[Boolean] =
    fetchAllHomesForPlayer(uuid).map(_.contains(name))

  override def saveHome(home: Home): Future[Unit] = saveHomeData(home.owner, modifyHomes = _.updated(home.name, home))

  override def deleteSavedHome(uuid: UUID, name: String): Future[Unit] =
    saveHomeData(uuid, modifyHomes = _.removed(name))

  override def searchHomes(
      location: Location,
      radius: Option[Double],
      world: Option[UUID],
      owner: Option[UUID],
      drop: Int,
      take: Int
  ): FutureOrNow[Seq[Home]] =
    FutureOrNow
      .now(owner.fold(homeOwners)(id => Set(id)))
      .flatMap(owners => owners.toSeq.traverse(allHomesForPlayer))
      .map(_.flatMap(_.values.filter { home =>
        world.forall(_ == home.worldUuid) && radius.forall { r =>
          inline def square(d: Double): Double = d * d
          square(home.x - location.getX) + square(home.y - location.getY) + square(home.z - location.getZ) < r * r
        }
      }))

  override def fetchAllResidentsForPlayer(homeOwner: UUID): Future[Map[String, Set[UUID]]] =
    fetchHomeDataForPlayer(homeOwner).map(_._2.map(t => t._1 -> t._2.map(_.resident)))

  override def fetchGetHomeResidents(homeOwner: UUID, homeName: String): Future[Set[UUID]] =
    fetchAllResidentsForPlayer(homeOwner).map(_.getOrElse(homeName, Set.empty))

  override def fetchIsPlayerResident(homeOwner: UUID, homeName: String, player: UUID): Future[Boolean] =
    fetchAllResidentsForPlayer(homeOwner).map(_.getOrElse(homeName, Set.empty).contains(player))

  override def saveResident(resident: Resident): Future[Unit] =
    saveHomeData(
      resident.owner,
      modifyResidents = m => m.updated(resident.homeName, m.getOrElse(resident.homeName, Set.empty) + resident)
    )

  override def removeSavedResident(homeOwner: UUID, homeName: String, resident: UUID): Future[Unit] =
    saveHomeData(
      homeOwner,
      modifyResidents = m => m.updated(homeName, m.getOrElse(homeName, Set.empty).filter(_.resident != resident))
    )

  protected val homeOwnersCache: CachedRemoteData[Set[UUID]] = CachedRemoteData.default[Set[UUID]](
    () =>
      FutureOrNow.fromFuture(Future {
        Files
          .list(storagePath)
          .toScala(Set)
          .flatMap(path => Try(UUID.fromString(path.getFileName.toString.stripSuffix(".json"))).toOption)
      }),
    30.seconds
  )

  override def exportData(): FutureOrNow[(Seq[Home], Seq[Resident])] =
    FutureOrNow
      .now(homeOwners)
      .flatMap { players =>
        FutureOrNow.fromFuture(
          players.toSeq.traverse(player =>
            fetchHomeDataForPlayer(player).map(t => (t._1.values.toSeq, t._2.values.toSeq.flatten))
          )
        )
      }
      .map(seq => (seq.flatMap(_._1), seq.flatMap(_._2)))

  override def importData(homes: Seq[Home], residents: Seq[Resident]): FutureOrNow[Unit] =
    val homesByOwner     = homes.groupBy(_.owner)
    val residentsByOwner = residents.groupBy(_.owner)

    FutureOrNow.fromFuture(
      homesByOwner.toSeq.traverse { case (owner, homes) =>
        val residents = residentsByOwner.getOrElse(owner, Seq.empty)

        saveHomeData(
          owner,
          modifyHomes = _ => homes.groupMapReduce(_.name)(identity)((_, b) => b),
          modifyResidents = _ => residents.groupMap(_.homeName)(identity).map(t => t._1 -> t._2.toSet)
        )
      }.void
    )

  @EventHandler(ignoreCancelled = true) override def onPlayerJoin(event: PlayerJoinEvent): Unit =
    super.onPlayerJoin(event)

  @EventHandler(ignoreCancelled = true) override def onPlayerLeave(event: PlayerQuitEvent): Unit =
    super.onPlayerLeave(event)
}
