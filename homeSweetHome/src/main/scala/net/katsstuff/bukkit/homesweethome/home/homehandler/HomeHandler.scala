package net.katsstuff.bukkit.homesweethome.home.homehandler

import java.nio.file.Files
import java.util.UUID

import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.CollectionConverters.*

import cats.syntax.all.*
import io.circe.*
import io.circe.syntax.*
import net.katsstuff.bukkit.homesweethome.home.storage.HomeStorage
import net.katsstuff.bukkit.homesweethome.home.{Home, Resident}
import net.katsstuff.bukkit.katlib.GlobalPlayer
import net.katsstuff.bukkit.katlib.util.FutureOrNow
import org.bukkit.{Bukkit, OfflinePlayer, World}

trait HomeHandler(storage: HomeStorage) {
  export storage.*

  def reload(): Future[Unit]

  def globalOnlinePlayers: Seq[GlobalPlayer]

  /** Add a home request */
  def addRequest(requester: UUID, homeOwner: UUID, home: Home): FutureOrNow[Unit]

  /** Removed a home request */
  def removeRequest(requester: UUID, homeOwner: UUID): FutureOrNow[Unit]

  /** Get a home request */
  def getRequest(requester: UUID, homeOwner: UUID): Option[Home]

  /** Get all the requesters for a single home owner. */
  def getAllRequestersForPlayer(homeOwner: UUID): Seq[UUID]

  /** Add a new invite to a specific home for a homeowner */
  def addInvite(target: UUID, homeOwner: UUID, home: Home): FutureOrNow[Unit]

  /** Removed an invite */
  def removeInvite(player: UUID, homeOwner: UUID): FutureOrNow[Unit]

  /** Check if a player is invited to a specific home */
  def isInvited(target: UUID, homeOwner: UUID, home: Home): Boolean

  /** The amount of homes a player can have */
  def getHomeLimit(world: World, player: OfflinePlayer): Int

  /** The amount of residents a home for a player can have */
  def getResidentLimit(world: World, player: OfflinePlayer): Int

  def exportStorageData()(using ExecutionContext): FutureOrNow[Unit] =
    storage.exportData().flatMap { (homes, residents) =>
      val str = Json.obj("homes" := homes, "residents" := residents).noSpaces
      FutureOrNow.fromFuture(
        Future {
          Files.write(storage.exportImportPath, str.linesIterator.toSeq.asJava)
          ()
        }
      )
    }

  def importStorageData()(using ExecutionContext): FutureOrNow[Unit] =
    FutureOrNow
      .fromFuture(Future(Files.readAllLines(storage.exportImportPath).asScala.mkString("\n")))
      .flatMap { str =>
        FutureOrNow.fromFuture(
          Future.fromTry(
            parser
              .parse(str)
              .flatMap(json =>
                json.hcursor.get[Seq[Home]]("homes").product(json.hcursor.get[Seq[Resident]]("residents"))
              )
              .toTry
          )
        )
      }
      .flatMap(data => storage.importData(data._1, data._2))
}
