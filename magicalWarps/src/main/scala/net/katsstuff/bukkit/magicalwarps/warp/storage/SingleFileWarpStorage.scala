package net.katsstuff.bukkit.magicalwarps.warp.storage

import java.nio.file.{Files, Path}

import scala.collection.mutable
import scala.concurrent.Future
import scala.jdk.CollectionConverters.*

import io.circe.syntax.*
import io.circe.{Json, Printer, parser}
import net.katsstuff.bukkit.katlib.util.FutureOrNow
import net.katsstuff.bukkit.magicalwarps.warp.{OldWarp, Warp}
import net.katsstuff.bukkit.magicalwarps.{DynmapInterop, WarpsConfig, WarpsPlugin}
import org.bukkit.Bukkit

class SingleFileWarpStorage(storagePath: Path)(using plugin: WarpsPlugin, config: WarpsConfig) extends WarpStorage {

  private val warpMap = new mutable.HashMap[String, Warp]

  private def dynmapLoaded: Boolean = Bukkit.getPluginManager.isPluginEnabled("dynmap")

  override def reloadWarps(): Future[Unit] =
    plugin.logger.info("Loading warps")
    Future {
      val warpsToAdd = if Files.exists(storagePath) then
        val content = Files.readAllLines(storagePath).asScala.mkString("\n")

        (for
          json <- parser.parse(content)
          cursor = json.hcursor
          version <- cursor.get[Int]("version")
          res <- version match
            case 3 =>
              cursor.get[Map[String, Warp]]("warp")
            case 2 =>
              cursor
                .get[Map[String, OldWarp]]("warp")
                .map(_.map(t => (t._1, t._2.toWarpK(t._1))))
            case _ => Left(new Exception("Unsupported version"))
        yield res.map((k, v) => (k, v.copy(groups = v.groups.filter(g => g != "all" && g.nonEmpty)): Warp))).toTry.get
      else
        plugin.logger.info("No warps found")
        Seq.empty

      warpMap.clear()
      warpMap ++= warpsToAdd

      if dynmapLoaded then
        DynmapInterop.deleteMarkerSet()
        warpsToAdd.foreach { case (_, warp) => addMarker(warp) }
    }(scala.concurrent.ExecutionContext.global)

  override def allWarps: Map[String, Warp] = warpMap.toMap

  override def getWarp(name: String): Option[Warp] = allWarps.get(lowercase(name))

  override def setWarp(warp: Warp): FutureOrNow[Unit] =
    val oldWarp = warpMap.get(warp.name)
    warpMap.update(warp.name, warp)
    oldWarp.foreach(w => removeMarker(w.name))
    removeMarker(warp.name)
    addMarker(warp)
    save()

  override def removeWarp(name: String): FutureOrNow[Unit] =
    warpMap.remove(name)
    removeMarker(name)
    save()

  override def exportImportPath: Path = plugin.exportImportPath

  override def exportData(): FutureOrNow[Seq[Warp]] =
    FutureOrNow.now(warpMap.values.toSeq)

  override def importData(warps: Seq[Warp]): FutureOrNow[Unit] =
    warpMap.clear()
    warpMap ++= warps.map(w => (w.name, w))

    DynmapInterop.deleteMarkerSet()
    warps.foreach(addMarker)

    save()

  private def addMarker(warp: Warp): Unit =
    if dynmapLoaded then DynmapInterop.addMarker(warp)

  private def removeMarker(name: String): Unit =
    if dynmapLoaded then DynmapInterop.removeMarker(name)

  private def save(): FutureOrNow[Unit] = {
    val res = Future {
      val printer = Printer.noSpaces.copy(dropNullValues = true)
      val json    = Json.obj("version" -> 3.asJson, "warp" -> warpMap.asJson)

      Files.createDirectories(storagePath.getParent)
      Files.write(storagePath, json.printWith(printer).linesIterator.toSeq.asJava)
      ()
    }(scala.concurrent.ExecutionContext.global)

    res.failed.foreach { e =>
      plugin.logger.error("Couldn't save warps", e)
      Left(e)
    }(scala.concurrent.ExecutionContext.global)

    FutureOrNow.fromFuture(res)
  }
}
