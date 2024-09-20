package net.katsstuff.bukkit.homesweethome.home.storage

import java.nio.file.Path
import java.time.Instant
import java.util.UUID
import java.util.concurrent.TimeUnit

import scala.collection.mutable
import scala.concurrent.Future

import com.google.common.cache.CacheBuilder
import net.katsstuff.bukkit.homesweethome.HSHConfig.HomeConfig
import net.katsstuff.bukkit.homesweethome.home.{Home, HomeK, Resident, ResidentK}
import net.katsstuff.bukkit.homesweethome.lib.LibPerm
import net.katsstuff.bukkit.katlib.util.FutureOrNow
import org.bukkit.entity.Player
import org.bukkit.{Bukkit, Location, OfflinePlayer, World}

trait HomeStorage {

  /** Clears the current homes and reloads them from disk. */
  def reloadHomeData(): Future[Unit]

  /**
    * Gets all the homes for a specific player.
    *
    * @return
    *   A map containing all the homes for a specific player. The map itself is
    *   a copy.
    */
  def allHomesForPlayer(homeOwner: UUID): FutureOrNow[Map[String, Home]]

  /**
    * Gets a specific home for a player.
    *
    * @param homeOwner
    *   UUID of player
    * @param homeName
    *   Name of player
    * @return
    *   Home if it was found
    */
  def specificHome(homeOwner: UUID, homeName: String): FutureOrNow[Option[Home]]

  /**
    * Check how many homes a user has.
    * @param homeOwner
    *   The user to check for.
    */
  def homeCount(homeOwner: UUID): FutureOrNow[Int]

  /**
    * Check if a player has a home with the specific name.
    *
    * @param homeOwner
    *   UUID of player
    * @param homeName
    *   Name of home
    * @return
    *   If home exist
    */
  def homeExist(homeOwner: UUID, homeName: String): FutureOrNow[Boolean]

  /**
    * Makes a new home for a player with a specific location.
    *
    * @param homeOwner
    *   The UUID of the player
    * @param homeName
    *   The name of the new home
    * @param location
    *   The location of the new home
    */
  def makeHome(homeOwner: UUID, homeName: String, location: Location): FutureOrNow[Unit]

  /**
    * Deletes a home with the specific name.
    *
    * @param homeOwner
    *   The UUID if the player to delete the home for.
    * @param homeName
    *   The name of the home to delete.
    */
  def deleteHome(homeOwner: UUID, homeName: String): FutureOrNow[Unit]

  /**
    * Search for homes that match the given criteria.
    * @param location
    *   The location to do the search relative to.
    * @param radius
    *   Radius from location to search for home.
    * @param world
    *   World to search for home in.
    * @param owner
    *   Owner to search for homes for.
    * @param drop
    *   How many homes to skip in the results.
    * @param take
    *   How many homes to take in the results.
    */
  def searchHomes(
      location: Location,
      radius: Option[Double] = None,
      world: Option[UUID] = None,
      owner: Option[UUID] = None,
      drop: Int = 0,
      take: Int = -1
  ): FutureOrNow[Seq[Home]]

  /** All the current known home owners. */
  def homeOwners: Set[UUID]

  /** All the current known home owners as offline players. */
  def homeOwnersPlayers: Map[String, OfflinePlayer]

  /**
    * Get all the residents of all homes for a player.
    * @param homeOwner
    *   The uuid of the player to get the residents for.
    */
  def allResidentsForPlayer(homeOwner: UUID): FutureOrNow[Map[String, Set[UUID]]]

  /**
    * All the residents of a home.
    * @param homeOwner
    *   UUID of the home owner.
    * @param homeName
    *   Name of the home
    */
  def getHomeResidents(homeOwner: UUID, homeName: String): FutureOrNow[Set[UUID]]

  /**
    * Check if a player is a resident of a home.
    * @param homeOwner
    *   The home owner uuid.
    * @param homeName
    *   The name of the home.
    * @param player
    *   The player uuid.
    */
  def isPlayerResident(homeOwner: UUID, homeName: String, player: UUID): FutureOrNow[Boolean]

  /**
    * Add a player as a resident of a home.
    * @param homeOwner
    *   The owner of the home.
    * @param homeName
    *   The name of the home.
    * @param resident
    *   The uuid of the player to add as a resident.
    */
  def addResident(homeOwner: UUID, homeName: String, resident: UUID): FutureOrNow[Unit]

  /**
    * Removes a player as a resident of a home.
    * @param homeOwner
    *   The owner of the home.
    * @param homeName
    *   The name of the home.
    * @param resident
    *   The uuid of the player to remove as a resident.
    */
  def removeResident(homeOwner: UUID, homeName: String, resident: UUID): FutureOrNow[Unit]

  /** Where data should be exported to and imported from. */
  def exportImportPath: Path

  /** Export all saved data. */
  def exportData(): FutureOrNow[(Seq[Home], Seq[Resident])]

  /**
    * Import data into storage.
    * @param homes
    *   The homes to import.
    * @param residents
    *   The residents to import.
    */
  def importData(homes: Seq[Home], residents: Seq[Resident]): FutureOrNow[Unit]
}
