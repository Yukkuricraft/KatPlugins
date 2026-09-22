package net.katsstuff.bukkit.katlib.testing

import net.katsstuff.bukkit.katlib.ScalaPlugin

/** A plugin that only does KatLib's own setup. */
class TestScalaPlugin extends ScalaPlugin:
  override def onEnable(): Unit = runKatLibRepeatableSetup()
