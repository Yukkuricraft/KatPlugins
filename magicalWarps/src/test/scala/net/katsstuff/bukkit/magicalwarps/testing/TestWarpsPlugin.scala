package net.katsstuff.bukkit.magicalwarps.testing

import net.katsstuff.bukkit.magicalwarps.WarpsPlugin

/** MagicalWarps, but only doing KatLib's setup when enabled. */
class TestWarpsPlugin extends WarpsPlugin:
  override def onEnable(): Unit = runKatLibRepeatableSetup()
