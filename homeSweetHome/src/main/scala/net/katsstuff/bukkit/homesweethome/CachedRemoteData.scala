package net.katsstuff.bukkit.homesweethome

import scala.collection.mutable
import scala.compiletime.uninitialized
import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext}

import cats.effect.IO
import cats.effect.kernel.Resource
import io.circe.*
import net.katsstuff.bukkit.katlib.ScalaPlugin
import net.katsstuff.bukkit.katlib.util.FutureOrNow
import org.bukkit.Bukkit
import skunk.Session
import skunk.data.Identifier

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

  private case class CachedPostgresUpdate(action: String, payload: Json) derives Decoder

  private class PostgresCached[A <: AnyRef](
      fetchData: () => FutureOrNow[A],
      refreshTime: FiniteDuration,
      postgresChannel: String,
      sessionPool: Resource[IO, Session[IO]],
      onCreate: Option[(A, Json) => Either[DecodingFailure, A]] = None,
      onUpdate: Option[(A, Json) => Either[DecodingFailure, A]] = None,
      onDelete: Option[(A, Json) => Either[DecodingFailure, A]] = None,
      customOperations: Map[String, (A, Json) => Either[DecodingFailure, A]] = Map.empty
  )(using sc: HomePlugin)(
      using ExecutionContext
  ) extends CachedRemoteData[A](fetchData, refreshTime) {
    private var closeIo: IO[Unit] = uninitialized
    startListenForNotify()

    private val allOperations = Seq("CREATE" -> onCreate, "UPDATE" -> onUpdate, "DELETE" -> onDelete)
      .collect { case (k, Some(f)) =>
        k -> f
      }
      .foldLeft(customOperations) { case (acc, (k, f)) =>
        acc.updated(k, f)
      }

    private def startListenForNotify(): Unit = {
      val (stream, close) = sc.dispatcher.unsafeRunSync(
        for
          t1 <- sessionPool.allocated
          (session, close1) = t1
          t2 <- session.channel(Identifier.fromString(postgresChannel).toOption.get).listenR(512).allocated
          (stream, close2) = t2
        yield (stream, close2 *> close1)
      )
      
      closeIo = close

      sc.dispatcher.unsafeRunAndForget(
        stream
          .foreach { notification =>
            parser.decode[CachedPostgresUpdate](notification.value) match {
              case Right(CachedPostgresUpdate(operation, payload)) =>
                IO(
                  if get != null then
                    allOperations.get(operation).flatMap(f => f(get, payload).toOption).fold(refreshNow())(setData)
                  else refreshNow()
                )

              case Left(e) => IO(sc.logger.error(e.getMessage, e))
            }
          }
          .compile
          .drain
      )
    }

    override def close(): Unit =
      super.close()
      sc.dispatcher.unsafeRunSync(closeIo)
  }

  def postgresNotify[A <: AnyRef](
      fetchData: () => FutureOrNow[A],
      refreshTime: FiniteDuration,
      postgresChannel: String,
      sessionPool: Resource[IO, Session[IO]],
      onCreate: Option[(A, Json) => Either[DecodingFailure, A]] = None,
      onUpdate: Option[(A, Json) => Either[DecodingFailure, A]] = None,
      onDelete: Option[(A, Json) => Either[DecodingFailure, A]] = None,
      customOperations: Map[String, (A, Json) => Either[DecodingFailure, A]] = Map.empty
  )(
      using HomePlugin,
      ExecutionContext
  ): CachedRemoteData[A] =
    new PostgresCached(
      fetchData,
      refreshTime,
      postgresChannel,
      sessionPool,
      onCreate,
      onUpdate,
      onDelete,
      customOperations
    )
