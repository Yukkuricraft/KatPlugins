package net.katsstuff.bukkit.homesweethome.home.storage

import java.nio.file.{Files, Path}
import java.time.Instant
import java.util.UUID

import scala.collection.mutable
import scala.concurrent.Future
import scala.jdk.CollectionConverters.*

import cats.syntax.all.*
import io.circe.*
import io.circe.syntax.*
import net.katsstuff.bukkit.homesweethome.home.{Home, OldHome, Resident}
import net.katsstuff.bukkit.homesweethome.{HSHConfig, HomePlugin, NestedMap}
import net.katsstuff.bukkit.katlib.util.FutureOrNow
import org.bukkit.{Bukkit, Location, OfflinePlayer}
import perspective.Id

class SingleFileHomeStorage(storagePath: Path)(implicit plugin: HomePlugin, hshConfig: HSHConfig) extends HomeStorage {
  private val homeMap: NestedMap[UUID, String, Home] = NestedMap(mutable.HashMap.empty, () => mutable.HashMap.empty)
  private val residentsMap: NestedMap[UUID, String, Set[UUID]] =
    NestedMap(mutable.HashMap.empty, () => mutable.HashMap.empty)
  private var _homeOwnerPlayers: Map[String, OfflinePlayer] = Map.empty

  override def reloadHomeData(): Future[Unit] =
    plugin.logger.info("Loading homes")
    Future {
      val (homesToAdd, residentsToAdd) = if Files.exists(storagePath) then
        val content = Files.readAllLines(storagePath).asScala.mkString("\n")

        def flattenNested[A, B](
            nested: Map[UUID, Map[String, A]],
            f: (A, UUID, String) => B
        ): Iterable[(UUID, String, B)] =
          nested.flatMap((owner, homes) =>
            homes.filter(_._1.nonEmpty).map((name, home) => (owner, name, f(home, owner, name)))
          )

        (for
          json <- parser.parse(content)
          cursor = json.hcursor
          version <- cursor.get[Int]("version")
          res <- version match
            case 4 =>
              cursor
                .get[Map[UUID, Map[String, Home]]]("home")
                .map(flattenNested(_, (h, _, _) => h))
                .product(
                  cursor
                    .get[Map[UUID, Map[String, Set[UUID]]]]("residents")
                    .map(flattenNested(_, (h, _, _) => h))
                )
            case 3 =>
              cursor
                .get[Map[UUID, Map[String, OldHome]]]("home")
                .map { allHomes =>
                  (
                    flattenNested(allHomes, _.toHomeK(_, _)),
                    flattenNested(allHomes, (h, _, _) => h.residents)
                  )
                }
            case _ => Left(new Exception("Unsupported version"))
        yield res).toTry.get
      else
        plugin.logger.info("No homes found")
        (Seq.empty, Seq.empty)

      homeMap.clear()
      residentsMap.clear()
      homeMap ++= homesToAdd
      residentsMap ++= residentsToAdd

      _homeOwnerPlayers = homeOwners
        .map(Bukkit.getOfflinePlayer)
        .filter(_.getName != null)
        .map(p => p.getName -> p)
        .toMap
    }(scala.concurrent.ExecutionContext.global)

  override def allHomesForPlayer(homeOwner: UUID): FutureOrNow[Map[String, Home]] =
    FutureOrNow.now(homeMap.getAll(homeOwner))

  override def specificHome(homeOwner: UUID, homeName: String): FutureOrNow[Option[Home]] =
    FutureOrNow.now(homeMap.get(homeOwner, homeName))

  override def homeCount(homeOwner: UUID): FutureOrNow[Int] = FutureOrNow.now(homeMap.getAll(homeOwner).size)

  override def homeExist(homeOwner: UUID, homeName: String): FutureOrNow[Boolean] =
    FutureOrNow.now(homeMap.contains(homeOwner, homeName))

  override def makeHome(homeOwner: UUID, homeName: String, location: Location): FutureOrNow[Unit] =
    homeMap.put(homeOwner, homeName, Home.makeNew(homeOwner, homeName, location))

    val p = Bukkit.getOfflinePlayer(homeOwner)
    if p.getName != null then _homeOwnerPlayers = _homeOwnerPlayers.updated(p.getName, p)

    save()

  override def deleteHome(homeOwner: UUID, homeName: String): FutureOrNow[Unit] =
    homeMap.remove(homeOwner, homeName)

    if allHomesForPlayer(homeOwner).value.getOrElse(sys.error("Got future in SingleFileHomeStorage")).value.isEmpty then
      _homeOwnerPlayers = _homeOwnerPlayers.removed(Bukkit.getOfflinePlayer(homeOwner).getName)

    save()

  override def searchHomes(
      location: Location,
      radius: Option[Double],
      world: Option[UUID],
      owner: Option[UUID],
      drop: Int,
      take: Int
  ): FutureOrNow[Seq[Home]] =
    FutureOrNow.now(
      homeMap.toNormalMap
        .collect {
          case (ownerId, homes) if owner.forall(_ == ownerId) =>
            homes.values.filter { home =>
              world.forall(_ == home.worldUuid) && radius.forall { r =>
                inline def square(d: Double): Double = d * d
                square(home.x - location.getX) + square(home.y - location.getY) + square(home.z - location.getZ) < r * r
              }
            }
        }
        .flatten
        .toSeq
    )

  override def homeOwners: Set[UUID] = homeMap.toNormalMap.keySet

  override def allResidentsForPlayer(homeOwner: UUID): FutureOrNow[Map[String, Set[UUID]]] =
    FutureOrNow.now(residentsMap.getAll(homeOwner))

  override def getHomeResidents(homeOwner: UUID, homeName: String): FutureOrNow[Set[UUID]] =
    FutureOrNow.now(residentsMap.getOrElse(homeOwner, homeName, Set.empty))

  override def isPlayerResident(homeOwner: UUID, homeName: String, player: UUID): FutureOrNow[Boolean] =
    FutureOrNow.now(residentsMap.get(homeOwner, homeName).exists(_.contains(player)))

  override def addResident(homeOwner: UUID, homeName: String, resident: UUID): FutureOrNow[Unit] =
    if homeMap.contains(homeOwner, homeName) then
      val oldResidents = residentsMap.getOrElseUpdate(homeOwner, homeName, Set.empty)
      residentsMap.put(homeOwner, homeName, oldResidents + resident)

      save()
    else
      FutureOrNow.fromFuture(
        Future.failed(new Exception(s"Tried to add resident to non existent home $homeName"))
      )

  override def removeResident(homeOwner: UUID, homeName: String, resident: UUID): FutureOrNow[Unit] =
    if homeMap.contains(homeOwner, homeName) then
      val oldResidents = residentsMap.getOrElse(homeOwner, homeName, Set.empty)
      residentsMap.put(homeOwner, homeName, oldResidents - resident)

      save()
    else
      FutureOrNow.fromFuture(
        Future.failed(new Exception(s"Tried to remove a resident from non existent home $homeName"))
      )

  override def homeOwnersPlayers: Map[String, OfflinePlayer] = _homeOwnerPlayers

  override def exportImportPath: Path = plugin.exportImportPath

  override def exportData(): FutureOrNow[(Seq[Home], Seq[Resident])] =
    FutureOrNow.now(
      (
        homeMap.toNormalMap.values.flatMap(_.values).toSeq,
        residentsMap.toNormalMap
          .flatMap((owner, m) =>
            m.flatMap((name, residents) =>
              residents.map(resident => Resident[Id](owner, name, resident, Instant.now()))
            )
          )
          .toSeq
      )
    )

  override def importData(homes: Seq[Home], residents: Seq[Resident]): FutureOrNow[Unit] =
    homeMap.clear()
    homeMap ++= homes.map(h => (h.owner, h.name, h))

    residentsMap.clear()
    residentsMap ++= residents.groupMap(r => (r.owner, r.homeName))(_.resident).map(t => (t._1._1, t._1._2, t._2.toSet))

    save()

  private def save(): FutureOrNow[Unit] = {
    val res = Future {
      val printer = Printer.noSpaces.copy(dropNullValues = true)
      val json = Json.obj(
        "version"   -> 4.asJson,
        "home"      -> homeMap.toNormalMap.asJson,
        "residents" -> residentsMap.toNormalMap.asJson
      )

      Files.createDirectories(storagePath.getParent)
      Files.write(storagePath, json.printWith(printer).linesIterator.toSeq.asJava)
      ()
    }(scala.concurrent.ExecutionContext.global)

    res.failed.foreach { e =>
      plugin.logger.error("Couldn't save homes")
      e.printStackTrace()
    }(scala.concurrent.ExecutionContext.global)

    FutureOrNow.fromFuture(res)
  }
}
