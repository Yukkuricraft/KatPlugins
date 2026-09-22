package net.katsstuff.bukkit.magicalwarps.warp.storage

import java.nio.file.Path

import scala.concurrent.duration.*
import scala.concurrent.{ExecutionContext, Future}

import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.kernel.Resource
import dataprism.skunk.platform.PostgresSkunkPlatform.Api.*
import dataprism.skunk.sql.SkunkTypes.*
import dataprism.sql.*
import net.katsstuff.bukkit.katlib.db.PostgresCached
import net.katsstuff.bukkit.katlib.util.{CachedRemoteData, FutureOrNow}
import net.katsstuff.bukkit.magicalwarps.warp.{Warp, WarpK}
import net.katsstuff.bukkit.magicalwarps.{DynmapInterop, WarpsPlugin}
import org.bukkit.Bukkit
import skunk.{Codec, Session}

class PostgresWarpStorage(sessionPool: Resource[IO, Session[IO]])(
    using db: Db[Future, Codec],
    plugin: WarpsPlugin,
    ec: ExecutionContext
) extends WarpStorage,
      AutoCloseable {

  private val warpCache: CachedRemoteData[Map[String, Warp]] = PostgresCached.postgresNotify[Map[String, Warp]](
    () => FutureOrNow.fromFuture(fetchAllWarps()),
    60.seconds,
    "magicalwarps_warps_change",
    sessionPool,
    onCreate = Some((warps, json) => json.as[Warp].map(warp => warps.updated(warp.name, warp))),
    onUpdate = Some((warps, json) =>
      for
        oldWarp <- json.hcursor.get[Warp]("old")
        newWarp <- json.hcursor.get[Warp]("new")
      yield warps.removed(oldWarp.name).updated(newWarp.name, newWarp)
    ),
    onDelete = Some((warps, json) => json.as[Warp].map(warp => warps.removed(warp.name)))
  )

  // The warps we currently have Dynmap markers for. Only accessed from the server thread
  private var markedWarps: Map[String, Warp] = Map.empty

  warpCache.onChange(warps => runOnServerThread(updateMarkers(warps)))

  private def dynmapLoaded: Boolean = Bukkit.getPluginManager.isPluginEnabled("dynmap")

  private def fetchAllWarps(): Future[Map[String, Warp]] =
    Select(Query.from(WarpK.table)).run.map(_.map(warp => warp.name -> warp).toMap)

  private def logFailure[A](res: Future[A], message: String): FutureOrNow[A] =
    res.failed.foreach(e => plugin.logger.error(message, e))
    FutureOrNow.fromFuture(res)

  override def reloadWarps(): Future[Unit] = {
    plugin.logger.info("Loading warps from database")

    warpCache.refresh().asFuture.map { warps =>
      plugin.logger.info(s"Loaded ${warps.size} warps from database")
      runOnServerThread(resetMarkers(warps))
    }
  }

  override def allWarps: Map[String, Warp] = warpCache.get

  override def setWarp(warp: Warp): FutureOrNow[Unit] = {
    val res = Insert
      .into(WarpK.table)
      .values(warp)
      .onConflictUpdate(t => NonEmptyList.one(t.name))
      .run
      .map(_ => warpCache.modify(_.updated(warp.name, warp)))

    logFailure(res, s"Failed to save warp ${warp.name}")
  }

  override def removeWarp(name: String): FutureOrNow[Unit] = {
    val res = Delete
      .from(WarpK.table)
      .where(_.name === name.as(text))
      .run
      .map(_ => warpCache.modify(_.removed(name)))

    logFailure(res, s"Failed to remove warp $name")
  }

  override def renameWarp(warp: Warp, newName: String): FutureOrNow[Unit] = {
    val renamed: Warp = warp.copy(name = newName)

    // Insert before delete, so that a failure never loses the warp. No upsert, so we never overwrite another warp
    val res = for {
      _ <- Insert.into(WarpK.table).values(renamed).run
      _ <- Delete.from(WarpK.table).where(_.name === warp.name.as(text)).run
    } yield warpCache.modify(_.removed(warp.name).updated(newName, renamed))

    logFailure(res, s"Failed to rename warp ${warp.name} to $newName")
  }

  override def exportImportPath: Path = plugin.exportImportPath

  override def exportData(): FutureOrNow[Seq[Warp]] =
    logFailure(Select(Query.from(WarpK.table)).run, "Failed to export warps")

  override def importData(warps: Seq[Warp]): FutureOrNow[Unit] = {
    val res = for {
      _ <- Delete.from(WarpK.table).where(_ => DbValue.trueV).run
      _ <- warps.headOption.fold(Future.successful(0)) { head =>
        Insert.into(WarpK.table).values(head, warps.tail*).run
      }
      _ <- warpCache.refresh().asFuture
    } yield ()

    logFailure(res, "Failed to import warps")
  }

  override def close(): Unit = warpCache.close()

  private def runOnServerThread(action: => Unit): Unit =
    if Bukkit.isPrimaryThread then action
    else if plugin.isEnabled then plugin.serverThreadExecutionContext.execute(() => action)

  /**
    * Brings the Dynmap markers in line with the given warps, only touching the
    * ones that changed.
    */
  private def updateMarkers(warps: Map[String, Warp]): Unit =
    if dynmapLoaded then
      markedWarps.foreach { (name, warp) =>
        if !warps.get(name).contains(warp) then DynmapInterop.removeMarker(name)
      }
      warps.foreach { (name, warp) =>
        if !markedWarps.get(name).contains(warp) then DynmapInterop.addMarker(warp)
      }
      markedWarps = warps

  /** Recreates all the Dynmap markers from scratch. */
  private def resetMarkers(warps: Map[String, Warp]): Unit =
    if dynmapLoaded then
      DynmapInterop.deleteMarkerSet()
      markedWarps = Map.empty
      updateMarkers(warps)
}
