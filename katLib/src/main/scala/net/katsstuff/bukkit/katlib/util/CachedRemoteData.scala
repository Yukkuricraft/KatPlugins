package net.katsstuff.bukkit.katlib.util

import scala.collection.mutable
import scala.compiletime.uninitialized
import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext}

import net.katsstuff.bukkit.katlib.ScalaPlugin
import org.bukkit.Bukkit

class CachedRemoteData[A <: AnyRef](fetchData: () => FutureOrNow[A], refreshTime: FiniteDuration)(
    using sc: ScalaPlugin,
    ec: ExecutionContext
) extends AutoCloseable:
  private var currentData: A                                = uninitialized
  protected val mapped: mutable.Buffer[CachedRemoteData[?]] = mutable.Buffer()

  refreshNow()

  private val task =
    val ticksTime = refreshTime.toMillis / 20
    Bukkit.getScheduler.runTaskTimerAsynchronously(
      sc,
      new Runnable {
        override def run(): Unit = refreshNow()
      },
      ticksTime,
      ticksTime
    )

  def get: A = if currentData != null then currentData else Await.result(computeData().asFuture, 3.seconds)

  protected def setData(newData: A): Unit =
    currentData = newData
    mapped.foreach(_.refreshNow())

  private def computeData(): FutureOrNow[A] =
    val r = fetchData()
    r.foreach(setData)

    r

  def refreshNow(): Unit =
    computeData()

  override def close(): Unit =
    task.cancel()
    mapped.foreach(_.close())

  def map[B <: AnyRef](f: A => B): CachedRemoteData[B] =
    val res = new CachedRemoteData[B](
      () => if currentData == null then computeData().map(f) else FutureOrNow.now(f(currentData)),
      refreshTime
    )
    mapped += res
    res

object CachedRemoteData:

  def default[A <: AnyRef](fetchData: () => FutureOrNow[A], refreshTime: FiniteDuration)(
      using ScalaPlugin,
      ExecutionContext
  ): CachedRemoteData[A] =
    new CachedRemoteData[A](fetchData, refreshTime)
