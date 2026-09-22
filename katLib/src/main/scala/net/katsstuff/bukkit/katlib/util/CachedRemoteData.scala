package net.katsstuff.bukkit.katlib.util

import java.util.concurrent.CopyOnWriteArrayList

import scala.collection.mutable
import scala.compiletime.uninitialized
import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext}
import scala.jdk.CollectionConverters.*

import net.katsstuff.bukkit.katlib.ScalaPlugin
import org.bukkit.Bukkit

class CachedRemoteData[A <: AnyRef](fetchData: () => FutureOrNow[A], refreshTime: FiniteDuration)(
    using sc: ScalaPlugin,
    ec: ExecutionContext
) extends AutoCloseable:
  @volatile private var currentData: A                      = uninitialized
  protected val mapped: mutable.Buffer[CachedRemoteData[?]] = mutable.Buffer()
  private val listeners                                     = new CopyOnWriteArrayList[A => Unit]()

  refreshNow()

  private val task =
    val ticksTime = refreshTime.toMillis / 50
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
    synchronized { currentData = newData }
    dataChanged(newData)

  private def dataChanged(newData: A): Unit =
    listeners.asScala.foreach(_(newData))
    mapped.foreach(_.refreshNow())

  /**
    * Calls f with the new data every time it changes. f can be called from any
    * thread.
    */
  def onChange(f: A => Unit): Unit = listeners.add(f)

  /**
    * Atomically transforms the current data.
    * @return
    *   false if no data has been loaded yet, or if f returned None.
    */
  def tryModify(f: A => Option[A]): Boolean =
    val newData = synchronized {
      Option(currentData).flatMap(f).map { data =>
        currentData = data
        data
      }
    }
    newData.foreach(dataChanged)
    newData.isDefined

  /** Atomically transforms the current data, if any has been loaded. */
  def modify(f: A => A): Unit = tryModify(a => Some(f(a)))

  private def computeData(): FutureOrNow[A] =
    val r = fetchData()
    r.foreach(setData)

    r

  /** Fetches the data again, completing with the new data. */
  def refresh(): FutureOrNow[A] = computeData()

  /** Fetches the data again in the background, logging any failure. */
  def refreshNow(): Unit =
    computeData().value.left.foreach(_.failed.foreach(e => sc.logger.error("Failed to refresh cached data", e)))

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
