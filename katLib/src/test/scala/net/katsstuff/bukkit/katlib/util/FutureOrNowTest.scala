package net.katsstuff.bukkit.katlib.util

import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future, Promise}

import cats.Eval

class FutureOrNowTest extends munit.FunSuite:
  given ExecutionContext = ExecutionContext.global

  private def result[A](f: FutureOrNow[A]): A = Await.result(f.asFuture, 5.seconds)

  test("now values stay now through map and flatMap") {
    val f = FutureOrNow.now(1).map(_ + 1).flatMap(i => FutureOrNow.now(i * 10))
    assert(f.isNow)
    assertEquals(result(f), 20)
  }

  test("flatMap into a future gives a future") {
    val f = FutureOrNow.now(1).flatMap(i => FutureOrNow.fromFuture(Future(i + 1)))
    assert(!f.isNow)
    assertEquals(result(f), 2)
  }

  test("futures stay futures") {
    val p = Promise[Int]()
    val f = FutureOrNow.fromFuture(p.future).map(_ + 1)
    assert(!f.isNow)
    p.success(1)
    assertEquals(result(f), 2)
  }

  test("failed futures propagate") {
    val f = FutureOrNow.fromFuture(Future.failed[Int](new Exception("Expected"))).map(_ + 1)
    intercept[Exception](result(f))
  }

  test("foreach runs immediately for now values") {
    var ran = false
    FutureOrNow.now(1).foreach(_ => ran = true)
    assert(ran)
  }

  test("zip of two now values is now") {
    val f = FutureOrNow.now(1).zip(FutureOrNow.now("a"))
    assert(f.isNow)
    assertEquals(result(f), (1, "a"))
  }

  test("zip with a future is a future") {
    val f = FutureOrNow.now(1).zip(FutureOrNow.fromFuture(Future("a")))
    assert(!f.isNow)
    assertEquals(result(f), (1, "a"))
  }

  test("sequence of now values is now") {
    val f = FutureOrNow.sequence(List(FutureOrNow.now(1), FutureOrNow.now(2)))
    assert(f.isNow)
    assertEquals(result(f), List(1, 2))
  }

  test("sequence with a future is a future") {
    val f = FutureOrNow.sequence(List(FutureOrNow.now(1), FutureOrNow.fromFuture(Future(2))))
    assert(!f.isNow)
    assertEquals(result(f), List(1, 2))
  }

  test("memoize only evaluates once") {
    var count = 0
    val f     = FutureOrNow.fromEval(Eval.always { count += 1; count }).memoize
    assertEquals(result(f), 1)
    assertEquals(result(f), 1)
    assertEquals(count, 1)
  }
