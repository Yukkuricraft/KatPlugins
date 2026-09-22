package net.katsstuff.bukkit.homesweethome.testing

import net.katsstuff.bukkit.homesweethome.HomePlugin

/** HomeSweetHome, but only doing KatLib's setup when enabled. */
class TestHomePlugin extends HomePlugin:
  override def onEnable(): Unit = runKatLibRepeatableSetup()
