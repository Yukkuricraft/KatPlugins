package net.katsstuff.bukkit.katlib.db.testing

import net.katsstuff.bukkit.katlib.ScalaPlugin
import net.katsstuff.bukkit.katlib.db.ScalaDbPlugin

/** A plugin that only does KatLib's own setup, including the dispatcher. */
class TestDbPlugin extends ScalaPlugin, ScalaDbPlugin:
  override def onEnable(): Unit = runKatLibRepeatableSetup()
