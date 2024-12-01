package net.katsstuff.bukkit.homesweethome.home.storage

import java.nio.file.Path
import java.time.Instant
import java.util.UUID
import java.util.concurrent.TimeUnit

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.CollectionConverters.*

import com.google.common.cache.CacheBuilder
import net.katsstuff.bukkit.homesweethome.home.{Home, HomeK, Resident}
import net.katsstuff.bukkit.homesweethome.{HSHConfig, HomePlugin, NestedMap}
import net.katsstuff.bukkit.katlib.util.{CachedRemoteData, FutureOrNow}
import org.bukkit.event.player.{PlayerJoinEvent, PlayerQuitEvent}
import org.bukkit.event.{EventHandler, Listener}
import org.bukkit.{Bukkit, Location, OfflinePlayer}
import perspective.Id

trait CachedHomeStorage(implicit plugin: HomePlugin, ec: ExecutionContext, hshConfig: HSHConfig)
    extends HomeStorage
    with Listener
    with AutoCloseable {
  protected val homeMap: NestedMap[UUID, String, Home] = NestedMap(mutable.HashMap.empty, () => mutable.HashMap.empty)
  protected val residentsMap: NestedMap[UUID, String, Set[UUID]] =
    NestedMap(mutable.HashMap.empty, () => mutable.HashMap.empty)

  override def reloadHomeData(): Future[Unit] =
    homeMap.clear()
    residentsMap.clear()

    def flattenNested[A, B](
        nested: Iterable[(UUID, Map[String, A])],
        f: (A, UUID, String) => B = (a: A, _: UUID, _: String) => a
    ): Iterable[(UUID, String, B)] =
      nested.flatMap((owner, homes) =>
        homes.filter(_._1.nonEmpty).map((name, home) => (owner, name, f(home, owner, name)))
      )

    plugin.logger.info("Loading homes")
    for
      onlinePlayerHomes <- Future.traverse(Bukkit.getOnlinePlayers.asScala) { player =>
        fetchAllHomesForPlayer(player.getUniqueId).map(player.getUniqueId -> _)
      }
      _ = homeMap ++= flattenNested(onlinePlayerHomes)
      onlineResidents <- Future.traverse(Bukkit.getOnlinePlayers.asScala) { player =>
        fetchAllResidentsForPlayer(player.getUniqueId).map(player.getUniqueId -> _)
      }
      _ = residentsMap ++= flattenNested(onlineResidents)
    yield ()

  protected val homeMapCache: mutable.Map[(UUID, String), Any] =
    CacheBuilder.newBuilder().expireAfterWrite(3, TimeUnit.SECONDS).build[(UUID, String), Any]().asMap().asScala

  private def homeMapImpl[A](owner: UUID, cacheKey: String)(ifPresent: => A)(ifMissing: => Future[A]): FutureOrNow[A] =
    if homeMap.containsOuter(owner) then FutureOrNow.now(ifPresent)
    else
      homeMapCache.get((owner, cacheKey)) match {
        case Some(v) => FutureOrNow.now(v.asInstanceOf[A])
        case None =>
          val f = ifMissing
          f.foreach(a => homeMapCache.put((owner, cacheKey), a))
          FutureOrNow.fromFuture(ifMissing)
      }

  def fetchAllHomesForPlayer(uuid: UUID): Future[Map[String, Home]]

  override def allHomesForPlayer(homeOwner: UUID): FutureOrNow[Map[String, Home]] =
    homeMapImpl(homeOwner, "allHomesForPlayer")(homeMap.getAll(homeOwner))(fetchAllHomesForPlayer(homeOwner))

  def fetchSpecificHome(uuid: UUID, name: String): Future[Option[Home]]

  override def specificHome(homeOwner: UUID, homeName: String): FutureOrNow[Option[Home]] =
    homeMapImpl(homeOwner, "specificHome")(homeMap.get(homeOwner, homeName))(fetchSpecificHome(homeOwner, homeName))

  def fetchHomeCount(uuid: UUID): Future[Int]

  override def homeCount(homeOwner: UUID): FutureOrNow[Int] =
    homeMapImpl(homeOwner, "homeCount")(homeMap.getAll(homeOwner).size)(fetchHomeCount(homeOwner))

  def fetchHomeExist(uuid: UUID, name: String): Future[Boolean]

  override def homeExist(homeOwner: UUID, homeName: String): FutureOrNow[Boolean] =
    homeMapImpl(homeOwner, "homeExist")(homeMap.contains(homeOwner, homeName))(fetchHomeExist(homeOwner, homeName))

  def saveHome(home: Home): Future[Unit]

  override def makeHome(homeOwner: UUID, homeName: String, location: Location): FutureOrNow[Unit] =
    val newHomeK = HomeK.makeNew(homeOwner, homeName, location)

    val res = saveHome(newHomeK)

    if homeMap.containsOuter(homeOwner) then homeMap.put(homeOwner, homeName, newHomeK)

    FutureOrNow.fromFuture(res)

  def deleteSavedHome(uuid: UUID, name: String): Future[Unit]

  override def deleteHome(homeOwner: UUID, homeName: String): FutureOrNow[Unit] =
    val res = deleteSavedHome(homeOwner, homeName)

    if homeMap.containsOuter(homeOwner) then homeMap.remove(homeOwner, homeName)

    FutureOrNow.fromFuture(res)

  def fetchAllResidentsForPlayer(homeOwner: UUID): Future[Map[String, Set[UUID]]]

  override def allResidentsForPlayer(homeOwner: UUID): FutureOrNow[Map[String, Set[UUID]]] =
    homeMapImpl(homeOwner, "allResidentsForPlayer")(residentsMap.getAll(homeOwner))(
      fetchAllResidentsForPlayer(homeOwner)
    )

  def fetchGetHomeResidents(homeOwner: UUID, homeName: String): Future[Set[UUID]]

  override def getHomeResidents(homeOwner: UUID, homeName: String): FutureOrNow[Set[UUID]] =
    homeMapImpl(homeOwner, "getHomeResidents")(residentsMap.getOrElse(homeOwner, homeName, Set.empty))(
      fetchGetHomeResidents(homeOwner, homeName)
    )

  def fetchIsPlayerResident(homeOwner: UUID, homeName: String, player: UUID): Future[Boolean]

  override def isPlayerResident(homeOwner: UUID, homeName: String, player: UUID): FutureOrNow[Boolean] =
    homeMapImpl(homeOwner, "isPlayerResident")(residentsMap.getOrElse(homeOwner, homeName, Set.empty).contains(player))(
      fetchIsPlayerResident(homeOwner, homeName, player)
    )

  def saveResident(resident: Resident): Future[Unit]

  override def addResident(homeOwner: UUID, homeName: String, resident: UUID): FutureOrNow[Unit] =
    val res = saveResident(Resident[Id](homeOwner, homeName, resident, Instant.now()))

    if homeMap.containsOuter(homeOwner) then
      residentsMap.update(homeOwner, homeName, residentsMap.getOrElse(homeOwner, homeName, Set.empty) + resident)

    FutureOrNow.fromFuture(res)

  def removeSavedResident(homeOwner: UUID, homeName: String, resident: UUID): Future[Unit]

  override def removeResident(homeOwner: UUID, homeName: String, resident: UUID): FutureOrNow[Unit] =
    val res = removeSavedResident(homeOwner, homeName, resident)

    if homeMap.containsOuter(homeOwner) then
      residentsMap.update(
        homeOwner,
        homeName,
        residentsMap.getOrElse(homeOwner, homeName, Set.empty).filter(_ != resident)
      )

    FutureOrNow.fromFuture(res)

  protected val homeOwnersCache: CachedRemoteData[Set[UUID]]
  protected lazy val homeOwnerPlayersCache: CachedRemoteData[Map[String, OfflinePlayer]] =
    homeOwnersCache.map { homeOwners =>
      homeOwners
        .map { uuid =>
          val p = Bukkit.getOfflinePlayer(uuid)
          p.getName -> p
        }
        .filter(_._1 != null)
        .toMap
    }

  override def close(): Unit = homeOwnersCache.close()

  override def homeOwners: Set[UUID] = homeOwnersCache.get

  override def homeOwnersPlayers: Map[String, OfflinePlayer] = homeOwnerPlayersCache.get

  override def exportImportPath: Path = plugin.exportImportPath

  @EventHandler(ignoreCancelled = true)
  def onPlayerJoin(event: PlayerJoinEvent): Unit =
    val uuid = event.getPlayer.getUniqueId
    homeMap.makeInnerIfNotExists(uuid)
    println(s"Adding ${event.getPlayer.getName} to player cache")

    val homesFut = fetchAllHomesForPlayer(uuid)
    homesFut.foreach { homes =>
      if event.getPlayer.isOnline then homeMap ++= homes.map(t => (uuid, t._1, t._2))
    }

    homesFut.failed.foreach(e => plugin.logger.error(e.getMessage, e))

    val residentsFut = fetchAllResidentsForPlayer(uuid)
    residentsFut.foreach { residents =>
      if event.getPlayer.isOnline then residentsMap ++= residents.map(t => (uuid, t._1, t._2))
    }

    residentsFut.failed.foreach(e => plugin.logger.error(e.getMessage, e))

  @EventHandler(ignoreCancelled = true)
  def onPlayerLeave(event: PlayerQuitEvent): Unit =
    homeMap.removeInner(event.getPlayer.getUniqueId)
    println(s"Removing ${event.getPlayer.getName} from player cache")
}
