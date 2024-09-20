package net.katsstuff.bukkit.magicalwarps

import scala.jdk.CollectionConverters.*

import net.katsstuff.bukkit.katlib.ScalaPlugin
import net.katsstuff.bukkit.magicalwarps.warp.Warp
import net.kyori.adventure.text.serializer.plain.PlainTextComponentSerializer
import org.bukkit.Bukkit
import org.dynmap.DynmapAPI

object DynmapInterop {

  private def dynmapApi = Bukkit.getPluginManager.getPlugin("dynmap").asInstanceOf[DynmapAPI]
  private def markerApi = dynmapApi.getMarkerAPI

  var hasWarned = false

  private def markerSet(using plugin: ScalaPlugin) =
    val setOpt = Option(markerApi.getMarkerSet("magicalwarps"))
      .orElse(Option(markerApi.createMarkerSet("magicalwarps", "MagicalWarps", null, false)))

    setOpt match
      case None =>
        if !hasWarned then plugin.logger.warn("Could not create marker set")

      case Some(set) =>
        Option(markerApi.getMarkerIcon("pin")).foreach { icon =>
          set.setDefaultMarkerIcon(icon)
        }

    setOpt
  end markerSet

  def deleteMarkerSet()(using plugin: ScalaPlugin): Unit =
    markerSet.foreach(_.getMarkers.asScala.foreach(_.deleteMarker()))

  def addMarker(warp: Warp)(using plugin: ScalaPlugin): Unit = {
    for
      set   <- markerSet
      world <- Option(Bukkit.getWorld(warp.world))
    do
      val icon = set.getDefaultMarkerIcon
      val marker =
        set.createMarker(warp.name, warp.stringDisplayName, world.getName, warp.x, warp.y, warp.z, icon, false)

      if (marker != null)
        warp.lore.foreach(lore => marker.setDescription(PlainTextComponentSerializer.plainText().serialize(lore)))
      else plugin.logger.warn(s"Couldn't create DynMap marker for $warp with name ${warp.name}")
  }

  def removeMarker(name: String)(using plugin: ScalaPlugin): Unit =
    markerSet.flatMap(set => Option(set.findMarker(name))).foreach(_.deleteMarker())
}
