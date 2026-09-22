package net.katsstuff.bukkit.katlib.db.testing

import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.Future
import scala.concurrent.duration.*

import cats.effect.std.Dispatcher
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Resource}
import cats.syntax.all.*
import dataprism.skunk.sql.SkunkSessionPoolDb
import dataprism.sql.Db
import fs2.io.net.Network
import natchez.Trace.Implicits.noop
import net.katsstuff.bukkit.katlib.db.{DbUpdates, PostgresSchema}
import org.testcontainers.postgresql.PostgreSQLContainer
import skunk.*
import skunk.codec.all.*
import skunk.data.Identifier
import skunk.implicits.*

/** A Postgres server shared by all tests in a JVM. Each test gets its own database. */
object TestPostgres:
  given Network[IO] = Network.forIO

  private lazy val container: PostgreSQLContainer =
    val c = new PostgreSQLContainer("postgres:17-alpine")
    c.start()
    sys.addShutdownHook(c.stop())
    c

  private val databaseCounter = new AtomicInteger()

  /** A fresh, empty database, with the schema already created unless told otherwise. */
  def freshDatabase(schema: String = "test_schema", createSchema: Boolean = true): TestDatabase =
    val name = s"test_db_${databaseCounter.incrementAndGet()}"
    val admin = TestDatabase(container.getDatabaseName, "public")
    admin.execute(sql"CREATE DATABASE #$name".command)
    val database = TestDatabase(name, schema)
    if createSchema then database.execute(sql"CREATE SCHEMA #$schema".command)
    database

  /** A database, connected to as the container's superuser unless another user is given. */
  case class TestDatabase(name: String, schema: String, login: Option[(String, String)] = None):
    def host: String     = container.getHost
    def port: Int        = container.getMappedPort(PostgreSQLContainer.POSTGRESQL_PORT)
    def user: String     = login.fold(container.getUsername)(_._1)
    def password: String = login.fold(container.getPassword)(_._2)

    def parameters: Map[String, String] = PostgresSchema.connectionParameters(schema, Map.empty)

    def single: Resource[IO, Session[IO]] =
      Session.single[IO](
        host = host,
        port = port,
        user = user,
        database = name,
        password = Some(password),
        parameters = Session.DefaultConnectionParameters ++ parameters
      )

    def pooled(max: Int = 10): Resource[IO, Resource[IO, Session[IO]]] =
      Session.pooled[IO](
        host = host,
        port = port,
        user = user,
        database = name,
        password = Some(password),
        max = max,
        parameters = Session.DefaultConnectionParameters ++ parameters
      )

    /** Runs a command on its own connection. */
    def execute(command: Command[Void]): Unit =
      single.use(_.execute(command)).void.unsafeRunSync()

    /** Runs a query on its own connection. */
    def query[A](query: Query[Void, A]): List[A] =
      single.use(_.execute(query)).unsafeRunSync()

    /** Runs a string of statements, split the same way as database updates. */
    def executeSql(sqlStr: String): Unit =
      single
        .use { s =>
          DbUpdates
            .splitStringIntoSqlStatements(sqlStr)
            .toList
            .traverse_(stmt => s.prepareR(sql"#$stmt".command).use(_.execute(Void)))
        }
        .unsafeRunSync()

    /** Sends a notification. */
    def notify(channel: String, payload: String): Unit =
      single
        .use(_.prepareR(sql"SELECT pg_notify($text, $text)::text".query(text)).use(_.unique((channel, payload))))
        .void
        .unsafeRunSync()

    /** The tables in the schema. */
    def tables: Set[String] =
      query(sql"SELECT table_name::text FROM information_schema.tables WHERE table_schema = current_schema()".query(text)).toSet

    def version: Option[Short] =
      query(sql"SELECT version FROM version".query(int2)).headOption

    /** Opens a pool and a Db the same way the plugins do. Closed by the returned action. */
    def connect(dispatcher: Dispatcher[IO], max: Int = 10): TestConnection =
      val (pool, close) = pooled(max).allocated.unsafeRunSync()
      val inUse         = new AtomicInteger()
      // Counted outside the pool's own resource, so a session is back in the pool once it's no longer counted
      val countedPool = Resource.make(IO(inUse.incrementAndGet()))(_ => IO(inUse.decrementAndGet()).void) *> pool
      val db: Db[Future, Codec] =
        SkunkSessionPoolDb[IO](countedPool).mapK([X] => (fx: IO[X]) => dispatcher.unsafeToFuture(fx))
      TestConnection(countedPool, db, inUse, close)

    /** Listens on a channel, collecting everything sent to it. */
    def listen(channel: String): TestListener =
      val received = new java.util.concurrent.CopyOnWriteArrayList[String]()
      val (stream, close) =
        (for
          session <- single
          stream  <- session.channel(Identifier.fromString(channel).toOption.get).listenR(512)
        yield stream).allocated.unsafeRunSync()
      val fiber = stream.foreach(n => IO(received.add(n.value)).void).compile.drain.start.unsafeRunSync()
      TestListener(received, fiber.cancel *> close)

  case class TestConnection(
      pool: Resource[IO, Session[IO]],
      db: Db[Future, Codec],
      inUse: AtomicInteger,
      closeIO: IO[Unit]
  ):
    /** How many sessions are taken from the pool right now. */
    def sessionsInUse: Int = inUse.get

    /** Closes the pool. Fails if sessions are still in use. */
    def close(): Unit = closeIO.unsafeRunSync()

  case class TestListener(received: java.util.List[String], closeIO: IO[Unit]):
    import scala.jdk.CollectionConverters.*
    def messages: Seq[String] = received.asScala.toSeq
    def close(): Unit        = closeIO.unsafeRunSync()
