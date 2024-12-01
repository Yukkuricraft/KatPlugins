package net.katsstuff.bukkit.katlib.db

import scala.collection.mutable
import scala.compiletime.uninitialized
import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext}

import cats.effect.IO
import cats.effect.kernel.Resource
import io.circe.*
import net.katsstuff.bukkit.katlib.ScalaPlugin
import net.katsstuff.bukkit.katlib.db.PostgresCached.CachedPostgresUpdate
import net.katsstuff.bukkit.katlib.util.{CachedRemoteData, FutureOrNow}
import org.bukkit.Bukkit
import skunk.Session
import skunk.data.Identifier

private class PostgresCached[A <: AnyRef](
    fetchData: () => FutureOrNow[A],
    refreshTime: FiniteDuration,
    postgresChannel: String,
    sessionPool: Resource[IO, Session[IO]],
    onCreate: Option[(A, Json) => Either[DecodingFailure, A]] = None,
    onUpdate: Option[(A, Json) => Either[DecodingFailure, A]] = None,
    onDelete: Option[(A, Json) => Either[DecodingFailure, A]] = None,
    customOperations: Map[String, (A, Json) => Either[DecodingFailure, A]] = Map.empty
)(using sc: ScalaDbPlugin)(
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
object PostgresCached:
  private case class CachedPostgresUpdate(action: String, payload: Json) derives Decoder

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
      using ScalaDbPlugin,
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
