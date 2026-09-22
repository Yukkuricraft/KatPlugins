package net.katsstuff.bukkit.katlib.util

import java.util.concurrent.CopyOnWriteArrayList
import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.duration.*
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.jdk.CollectionConverters.*

import net.katsstuff.bukkit.katlib.ScalaPlugin
import net.katsstuff.bukkit.katlib.testing.{BukkitSuite, TestScalaPlugin}

class CachedRemoteDataTest extends BukkitSuite:
  given ExecutionContext = ExecutionContext.global

  private def withPlugin[A](f: ScalaPlugin ?=> A): A =
    f(using createPlugin[ScalaPlugin]("Test", classOf[TestScalaPlugin]))

  /** A remote source whose value can be changed, counting the fetches. */
  class Source(initial: String):
    @volatile var value: String = initial
    val fetches                 = new AtomicInteger()
    def fetch(): FutureOrNow[String] =
      fetches.incrementAndGet()
      FutureOrNow.fromFuture(Future(value))

  test("fetches the data when created") {
    withPlugin {
      val source = Source("a")
      val cached = CachedRemoteData.default(() => source.fetch(), 1.hour)
      eventually()(cached.get == "a")
      assertEquals(source.fetches.get, 1)
      cached.close()
    }
  }

  test("get waits for the data if it isn't loaded yet") {
    withPlugin {
      val promise = Promise[String]()
      val cached  = CachedRemoteData.default(() => FutureOrNow.fromFuture(promise.future), 1.hour)
      Future { Thread.sleep(100); promise.success("a") }
      assertEquals(cached.get, "a")
      cached.close()
    }
  }

  test("modify does nothing before data is loaded") {
    withPlugin {
      val promise = Promise[String]()
      val cached  = CachedRemoteData.default(() => FutureOrNow.fromFuture(promise.future), 1.hour)
      assert(!cached.tryModify(s => Some(s + "b")))
      promise.success("a")
      eventually()(cached.tryModify(s => Some(s + "b")))
      assertEquals(cached.get, "ab")
      cached.close()
    }
  }

  test("tryModify returning None keeps the data") {
    withPlugin {
      val cached = CachedRemoteData.default(() => FutureOrNow.now("a"), 1.hour)
      assert(!cached.tryModify(_ => None))
      assertEquals(cached.get, "a")
      cached.close()
    }
  }

  test("modify changes the data and notifies listeners") {
    withPlugin {
      val cached  = CachedRemoteData.default(() => FutureOrNow.now("a"), 1.hour)
      val changes = new CopyOnWriteArrayList[String]()
      cached.onChange(changes.add(_))

      cached.modify(_ + "b")
      assertEquals(cached.get, "ab")
      assertEquals(changes.asScala.toSeq, Seq("ab"))
      cached.close()
    }
  }

  test("modifications from many threads are not lost") {
    withPlugin {
      val cached = CachedRemoteData.default[Vector[Int]](() => FutureOrNow.now(Vector.empty), 1.hour)
      val threads = (0 until 8).map { t =>
        new Thread(() => (0 until 100).foreach(i => cached.modify(_ :+ (t * 100 + i))))
      }
      threads.foreach(_.start())
      threads.foreach(_.join())
      assertEquals(cached.get.sorted, (0 until 800).toVector)
      cached.close()
    }
  }

  test("refresh fetches again and completes with the new data") {
    withPlugin {
      val source = Source("a")
      val cached = CachedRemoteData.default(() => source.fetch(), 1.hour)
      eventually()(cached.get == "a")

      source.value = "b"
      assertEquals(await(cached.refresh().asFuture), "b")
      assertEquals(cached.get, "b")
    }
  }

  test("refreshes periodically") {
    withPlugin {
      val source = Source("a")
      val cached = CachedRemoteData.default(() => source.fetch(), 1.second)
      eventually()(cached.get == "a")

      source.value = "b"
      server.getScheduler.performTicks(20)
      eventually()(cached.get == "b")
      cached.close()
    }
  }

  test("close stops the periodic refresh") {
    withPlugin {
      val source = Source("a")
      val cached = CachedRemoteData.default(() => source.fetch(), 1.second)
      eventually()(cached.get == "a")
      cached.close()

      val fetchesBefore = source.fetches.get
      server.getScheduler.performTicks(100)
      server.getScheduler.waitAsyncTasksFinished()
      assertEquals(source.fetches.get, fetchesBefore)
    }
  }

  test("a failed refresh keeps the old data") {
    withPlugin {
      @volatile var fail = false
      val cached = CachedRemoteData.default(
        () =>
          if fail then FutureOrNow.fromFuture(Future.failed(new Exception("Expected failure")))
          else FutureOrNow.now("a"),
        1.hour
      )
      fail = true
      cached.refreshNow()
      intercept[Exception](await(cached.refresh().asFuture))
      assertEquals(cached.get, "a")
      cached.close()
    }
  }

  test("mapped data follows the original") {
    withPlugin {
      val source = Source("a")
      val cached = CachedRemoteData.default(() => source.fetch(), 1.hour)
      eventually()(cached.get == "a")
      val mapped = cached.map(_.toUpperCase)
      eventually()(mapped.get == "A")

      cached.modify(_ + "b")
      eventually()(mapped.get == "AB")

      source.value = "c"
      await(cached.refresh().asFuture)
      eventually()(mapped.get == "C")
      cached.close()
    }
  }
