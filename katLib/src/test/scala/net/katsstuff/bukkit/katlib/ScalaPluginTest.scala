package net.katsstuff.bukkit.katlib

import scala.collection.mutable

import net.katsstuff.bukkit.katlib.testing.{BukkitSuite, TestScalaPlugin}
import org.bukkit.Bukkit

class ScalaPluginTest extends BukkitSuite:

  test("disable actions run newest first") {
    val plugin = createPlugin[ScalaPlugin]("Test", classOf[TestScalaPlugin])
    val ran    = mutable.Buffer[Int]()
    plugin.addDisableAction(ran += 1)
    plugin.addDisableAction(ran += 2)
    plugin.addDisableAction(ran += 3)

    plugin.runDisableActions()
    assertEquals(ran.toSeq, Seq(3, 2, 1))
  }

  test("a failing disable action does not stop the others") {
    val plugin = createPlugin[ScalaPlugin]("Test", classOf[TestScalaPlugin])
    val ran    = mutable.Buffer[Int]()
    plugin.addDisableAction(ran += 1)
    plugin.addDisableAction(throw new Exception("Expected failure"))
    plugin.addDisableAction(ran += 3)

    plugin.runDisableActions()
    assertEquals(ran.toSeq, Seq(3, 1))
  }

  test("disable actions only run once") {
    val plugin = createPlugin[ScalaPlugin]("Test", classOf[TestScalaPlugin])
    var count  = 0
    plugin.addDisableAction(count += 1)

    plugin.runDisableActions()
    plugin.runDisableActions()
    assertEquals(count, 1)
    assert(plugin.doWhenDisabling.isEmpty)
  }

  test("disabling the plugin runs the disable actions") {
    val plugin = createPlugin[ScalaPlugin]("Test", classOf[TestScalaPlugin])
    var ran    = false
    plugin.addDisableAction { ran = true }

    server.getPluginManager.disablePlugin(plugin)
    assert(ran)
  }

  test("serverThreadExecutionContext runs on the server thread") {
    val plugin              = createPlugin[ScalaPlugin]("Test", classOf[TestScalaPlugin])
    @volatile var onServer  = Option.empty[Boolean]
    val thread = new Thread(() => plugin.serverThreadExecutionContext.execute(() => onServer = Some(Bukkit.isPrimaryThread)))
    thread.start()
    thread.join()

    // Nothing runs until the server ticks
    assertEquals(onServer, None)
    eventually()(onServer.isDefined)
    assertEquals(onServer, Some(true))
  }
