package net.katsstuff.bukkit.magicalwarps.warp.storage

import java.nio.file.Path
import java.util.Locale

import scala.concurrent.Future

import net.katsstuff.bukkit.katlib.util.FutureOrNow
import net.katsstuff.bukkit.magicalwarps.lib.LibPerm
import net.katsstuff.bukkit.magicalwarps.warp.Warp
import net.milkbowl.vault.permission.Permission
import org.bukkit.Bukkit
import org.bukkit.command.CommandSender
import org.bukkit.entity.Player

trait WarpStorage {

  protected inline def lowercase(s: String): String = s.toLowerCase(Locale.ROOT)

  private lazy val perms: Permission = Bukkit.getServicesManager.load(classOf[Permission])

  def reloadWarps(): Future[Unit]

  def allWarps: Map[String, Warp]

  def groups: Seq[String] = allWarps.flatMap(_._2.groups).toSeq.distinct :+ "all"

  def getGroupWarps(group: String): Map[String, Warp] =
    if lowercase(group) == "all"
    then allWarps
    else allWarps.filter(_._2.groups.contains(group))

  def allAccessibleWarps(sender: CommandSender): Map[String, Warp] = sender match
    case _ if sender.hasPermission(LibPerm.IgnoreAllowed) => allWarps
    case player: Player                                   => allWarps.filter(t => canUseWarp(player, t._2))
    case _                                                => Map.empty

  def canUseWarp(sender: CommandSender, warp: Warp): Boolean =
    sender.hasPermission(LibPerm.IgnoreAllowed) || (sender match
      case player: Player =>
        (warp.allowedUsers.isEmpty && warp.allowedPermGroups.isEmpty) ||
        warp.allowedUsers.contains(player.getUniqueId) ||
        warp.allowedPermGroups.exists(perms.playerInGroup(player, _))

      case _ => false
    )

  /** Get a warp by name. */
  def getWarp(name: String): Option[Warp] = allWarps.get(lowercase(name))

  /** Set or update a warp */
  def setWarp(warp: Warp): FutureOrNow[Unit]

  /** Remove an existing warp. */
  def removeWarp(name: String): FutureOrNow[Unit]

  /** Where data should be exported to and imported from. */
  def exportImportPath: Path

  /** Export all saved data. */
  def exportData(): FutureOrNow[Seq[Warp]]

  /**
    * Import data into storage
    *
    * @param warps
    *   The warps to import.
    */
  def importData(warps: Seq[Warp]): FutureOrNow[Unit]
}
