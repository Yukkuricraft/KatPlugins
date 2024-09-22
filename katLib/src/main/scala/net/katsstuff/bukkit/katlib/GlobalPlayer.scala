package net.katsstuff.bukkit.katlib

import net.katsstuff.bukkit.katlib.text.*
import org.bukkit.entity.Player
import org.bukkit.{Bukkit, OfflinePlayer}

import java.util.UUID

enum GlobalPlayer:
  case OnThisServer(player: Player)
  case OnOtherServer(player: String, override val uuid: UUID)

  def sendMessage(sender: Player, message: Text)(using bungeeChannel: BungeeChannel): Unit =
    this match
      case OnThisServer(player)     => player.sendMessage(message)
      case OnOtherServer(player, _) => bungeeChannel.sendMessage(sender, player, message)

  def name: String = this match
    case OnThisServer(player)     => player.getName
    case OnOtherServer(player, _) => player

  def uuid: UUID = this match
    case OnThisServer(player)   => player.getUniqueId
    case OnOtherServer(_, uuid) => uuid

object GlobalPlayer:
  def ofOffline(player: OfflinePlayer): GlobalPlayer = {
    if player.isOnline then GlobalPlayer.OnThisServer(Bukkit.getPlayer(player.getUniqueId))
    else GlobalPlayer.OnOtherServer(player.getName, player.getUniqueId)
  }
