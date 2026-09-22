package net.katsstuff.bukkit.homesweethome

import scala.compiletime.uninitialized

import net.katsstuff.bukkit.homesweethome.home.homehandler.HomeHandler
import net.katsstuff.bukkit.katlib.BungeeChannel
import net.katsstuff.bukkit.katlib.util.Teleporter

final class HSHContext:
  @volatile var config: HSHConfig            = uninitialized
  @volatile var homeHandler: HomeHandler     = uninitialized
  @volatile var teleporter: Teleporter       = uninitialized
  @volatile var bungeeChannel: BungeeChannel = uninitialized
