package net.katsstuff.bukkit.katlib.testing

import java.util.concurrent.{Callable, FutureTask, Future as JFuture}

import be.seeseemelk.mockbukkit.ServerMock
import be.seeseemelk.mockbukkit.scheduler.BukkitSchedulerMock
import org.bukkit.plugin.Plugin

/** MockBukkit doesn't implement callSyncMethod, which KatLib uses to run code on the server thread. */
class TestSchedulerMock extends BukkitSchedulerMock:
  override def callSyncMethod[T](plugin: Plugin, task: Callable[T]): JFuture[T] =
    val future = new FutureTask[T](task)
    runTask(plugin, future)
    future

class TestServerMock extends ServerMock:
  private val scheduler = new TestSchedulerMock

  override def getScheduler: BukkitSchedulerMock = scheduler
