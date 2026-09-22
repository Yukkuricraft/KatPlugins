package net.katsstuff.bukkit.katlib.db.testing

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*

import net.katsstuff.bukkit.katlib.db.ScalaDbPlugin
import net.katsstuff.bukkit.katlib.db.testing.TestPostgres.{TestConnection, TestDatabase}
import net.katsstuff.bukkit.katlib.testing.BukkitSuite

/** A suite with a mocked server and a fresh database for each test. */
trait DbSuite extends BukkitSuite:
  given ExecutionContext = ExecutionContext.global

  private val toClose = mutable.Buffer[() => Unit]()

  /** Closes something after the test, before the server is unmocked. */
  def closeAfterTest(close: => Unit): Unit = toClose += (() => close)

  override def afterEach(context: AfterEach): Unit =
    try toClose.reverseIterator.foreach(close => close())
    finally
      toClose.clear()
      super.afterEach(context)

  def createDbPlugin(name: String = "Test"): ScalaDbPlugin =
    createPlugin[ScalaDbPlugin](name, classOf[TestDbPlugin])

  /**
    * Connects to the database the same way the plugins do, closing the
    * connection after the test. Before closing, work still running in the
    * background, like a refresh started right before the test ended, gets
    * some time to finish.
    */
  def connect(database: TestDatabase, plugin: ScalaDbPlugin): TestConnection =
    val connection = database.connect(plugin.dispatcher)
    closeAfterTest {
      val deadline = 10.seconds.fromNow
      while connection.sessionsInUse > 0 && !deadline.isOverdue() do
        server.getScheduler.performOneTick()
        Thread.sleep(10)
      assertEquals(connection.sessionsInUse, 0, "Sessions were still in use when the test ended. Is something not closed?")
      connection.close()
    }
    connection
