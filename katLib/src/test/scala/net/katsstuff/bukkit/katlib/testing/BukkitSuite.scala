package net.katsstuff.bukkit.katlib.testing

import scala.concurrent.duration.*
import scala.concurrent.{Await, Future}

import be.seeseemelk.mockbukkit.{MockBukkit, ServerMock}
import org.bukkit.plugin.java.JavaPlugin

/** A suite where every test gets a fresh mocked server. */
trait BukkitSuite extends munit.FunSuite:

  private var _server: ServerMock = null
  def server: ServerMock          = _server

  override def beforeEach(context: BeforeEach): Unit =
    super.beforeEach(context)
    _server = MockBukkit.mock(new TestServerMock)

  override def afterEach(context: AfterEach): Unit =
    try
      server.getPluginManager.disablePlugins()
      MockBukkit.unmock()
    finally super.afterEach(context)

  /**
    * Creates, enables and registers a plugin, which is disabled after the
    * test. See [[TestPluginClassLoader.createPlugin]].
    */
  def createPlugin[P <: JavaPlugin](name: String, pluginClass: Class[?]): P =
    val plugin = TestPluginClassLoader.createPlugin[P](name, pluginClass)
    server.getPluginManager.registerLoadedPlugin(plugin)
    plugin

  /** Waits for a future, running server ticks while waiting. */
  def await[A](future: Future[A], timeout: FiniteDuration = 10.seconds): A =
    eventually(timeout)(future.isCompleted)
    Await.result(future, Duration.Zero)

  /**
    * Runs server ticks until the condition holds, failing the test if it does
    * not hold before the timeout.
    */
  def eventually(timeout: FiniteDuration = 10.seconds)(condition: => Boolean): Unit =
    val deadline = timeout.fromNow
    while !condition do
      if deadline.isOverdue() then fail(s"Condition did not hold within $timeout")
      
      server.getScheduler.performOneTick()
      Thread.sleep(10)

  /** Like [[eventually]], but for assertions. */
  def eventuallyAssert(timeout: FiniteDuration = 10.seconds)(assertion: => Unit): Unit =
    val deadline                   = timeout.fromNow
    var lastError: Throwable | Null = null
    var done                       = false
    while !done do
      try
        assertion
        done = true
      catch
        case e: AssertionError =>
          lastError = e
          if deadline.isOverdue() then throw e
          
          server.getScheduler.performOneTick()
          Thread.sleep(10)
