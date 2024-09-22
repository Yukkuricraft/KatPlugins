package net.katsstuff.bukkit.homesweethome.home.homehandler

import java.io.{ByteArrayInputStream, DataInputStream}
import java.nio.file.{Files, Path}
import java.util.UUID
import java.util.concurrent.TimeUnit
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.CollectionConverters.*
import cats.syntax.all.*
import com.google.common.cache.CacheBuilder
import io.circe.*
import io.circe.syntax.*
import net.katsstuff.bukkit.homesweethome.home.storage.HomeStorage
import net.katsstuff.bukkit.homesweethome.home.{Home, Resident}
import net.katsstuff.bukkit.homesweethome.lib.LibPerm
import net.katsstuff.bukkit.homesweethome.{HSHConfig, NestedMap}
import net.katsstuff.bukkit.katlib.GlobalPlayer
import net.katsstuff.bukkit.katlib.util.FutureOrNow
import net.milkbowl.vault.chat.Chat
import org.bukkit.entity.Player
import org.bukkit.{Bukkit, OfflinePlayer, World}

/** The HomeHandler is what manages all the homes. */
class SingleServerHomeHandler(storage: HomeStorage, hshConfig: HSHConfig) extends HomeHandler(storage) {

  private def chat: Chat = Bukkit.getServicesManager.load(classOf[Chat])

  private val requests: NestedMap[UUID, UUID, Home] =
    NestedMap(mutable.HashMap.empty, () => createInvitesRequests)
  private val invites: NestedMap[UUID, UUID, Home] =
    NestedMap(mutable.HashMap.empty, () => createInvitesRequests)

  def globalOnlinePlayers: Seq[GlobalPlayer] = Bukkit.getOnlinePlayers.asScala.map(GlobalPlayer.OnThisServer(_)).toSeq

  /** Clears the current homes and reloads them from disk. */
  def reload(): Future[Unit] = {
    requests.clear()
    invites.clear()
    storage.reloadHomeData()
  }

  private def createInvitesRequests[A <: AnyRef, B <: AnyRef]: mutable.Map[A, B] =
    CacheBuilder
      .newBuilder()
      .expireAfterWrite(hshConfig.home.timeout, TimeUnit.SECONDS)
      .build[A, B]
      .asMap
      .asScala

  /** Add a home request */
  def addRequest(requester: UUID, homeOwner: UUID, home: Home): FutureOrNow[Unit] =
    FutureOrNow.now(requests.put(requester, homeOwner, home))

  /** Removed a home request */
  def removeRequest(requester: UUID, homeOwner: UUID): FutureOrNow[Unit] =
    FutureOrNow.now(requests.remove(requester, homeOwner))

  /** Get a home request */
  def getRequest(requester: UUID, homeOwner: UUID): Option[Home] =
    requests.get(requester, homeOwner)

  /** Get all the requesters for a single home owner. */
  def getAllRequestersForPlayer(homeOwner: UUID): Seq[UUID] = requests.toNormalMap
    .filter { case (_, inner) =>
      inner.contains(homeOwner)
    }
    .keys
    .toSeq

  /** Add a new invite to a specific home for a homeowner */
  def addInvite(target: UUID, homeOwner: UUID, home: Home): FutureOrNow[Unit] =
    FutureOrNow.now(invites.put(target, homeOwner, home))

  /** Removed an invite */
  def removeInvite(player: UUID, homeOwner: UUID): FutureOrNow[Unit] =
    FutureOrNow.now(invites.remove(player, homeOwner))

  /** Check if a player is invited to a specific home */
  def isInvited(target: UUID, homeOwner: UUID, home: Home): Boolean = invites.get(target, homeOwner).contains(home)

  /** The amount of homes a player can have */
  def getHomeLimit(world: World, player: OfflinePlayer): Int =
    chat.getPlayerInfoInteger(world.getName, player, LibPerm.HomeLimitOption, hshConfig.home.homeLimit)

  /** The amount of residents a home for a player can have */
  def getResidentLimit(world: World, player: OfflinePlayer): Int =
    chat.getPlayerInfoInteger(world.getName, player, LibPerm.ResidentLimitOption, hshConfig.home.residentLimit)
}
