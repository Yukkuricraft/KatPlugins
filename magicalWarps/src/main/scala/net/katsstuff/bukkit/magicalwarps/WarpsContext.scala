package net.katsstuff.bukkit.magicalwarps

import scala.compiletime.uninitialized

import net.katsstuff.bukkit.katlib.util.Teleporter
import net.katsstuff.bukkit.magicalwarps.warp.storage.WarpStorage

/**
  * Everything commands need that is replaced when the plugin reloads. Commands
  * are only created once, so they must read these from here when they run, and
  * never hold on to them.
  */
final class WarpsContext:
  @volatile var config: WarpsConfig    = uninitialized
  @volatile var storage: WarpStorage   = uninitialized
  @volatile var teleporter: Teleporter = uninitialized
