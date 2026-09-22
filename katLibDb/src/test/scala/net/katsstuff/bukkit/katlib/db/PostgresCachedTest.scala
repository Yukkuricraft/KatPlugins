package net.katsstuff.bukkit.katlib.db

import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.duration.*
import scala.concurrent.{ExecutionContext, Future}

import ch.qos.logback.classic.Level
import cats.effect.IO
import cats.effect.kernel.Resource
import dataprism.skunk.platform.PostgresSkunkPlatform.Api.*
import dataprism.skunk.sql.SkunkTypes.*
import dataprism.sql.*
import io.circe.Json
import net.katsstuff.bukkit.katlib.db.testing.TestPostgres.TestDatabase
import net.katsstuff.bukkit.katlib.db.testing.{DbSuite, NotifySql, TestPostgres}
import net.katsstuff.bukkit.katlib.testing.LogCapture
import net.katsstuff.bukkit.katlib.util.{CachedRemoteData, FutureOrNow}
import skunk.{Codec, Session}

class PostgresCachedTest extends DbSuite, LogCapture:

  private val channel = "test_items_change"

  private def setupDatabase(): TestDatabase =
    val database = TestPostgres.freshDatabase()
    database.executeSql(
      s"""${NotifySql.notifyTriggerFunction}
         |CREATE TABLE items(id int PRIMARY KEY, name text);
         |CREATE TRIGGER items_notify AFTER INSERT OR UPDATE OR DELETE ON items
         |    FOR EACH ROW EXECUTE PROCEDURE notify_trigger('$channel');""".stripMargin
    )
    database

  private def insert(database: TestDatabase, id: Int, name: String): Unit =
    database.executeSql(s"INSERT INTO items VALUES ($id, '$name')")

  class CachedItems(
      val database: TestDatabase,
      val cached: CachedRemoteData[Map[Int, String]],
      val fetches: AtomicInteger
  )

  private val decodeItem: Json => Either[io.circe.DecodingFailure, (Int, String)] = json =>
    for
      id   <- json.hcursor.get[Int]("id")
      name <- json.hcursor.get[String]("name")
    yield id -> name

  private def makeCached(
      database: TestDatabase,
      pluginName: String = "Test",
      withHandlers: Boolean = true
  ): CachedItems =
    given plugin: ScalaDbPlugin = createDbPlugin(pluginName)
    val connection              = connect(database, plugin)
    given Db[Future, Codec]     = connection.db
    val fetches                 = new AtomicInteger()

    val cached = PostgresCached.postgresNotify[Map[Int, String]](
      () =>
        fetches.incrementAndGet()
        FutureOrNow.fromFuture(
          connection.db
            .runIntoSimple(sql"SELECT id, name FROM items", skunk.codec.all.int4 ~ skunk.codec.all.text)
            .map(_.toMap)
        )
      ,
      1.hour,
      channel,
      connection.pool,
      onCreate = Option.when(withHandlers)((items, json) => decodeItem(json).map(items + _)),
      onUpdate = Option.when(withHandlers)((items, json) =>
        for
          oldItem <- decodeItem(json.hcursor.downField("old").focus.get)
          newItem <- decodeItem(json.hcursor.downField("new").focus.get)
        yield items - oldItem._1 + newItem
      ),
      onDelete = Option.when(withHandlers)((items, json) => json.hcursor.get[Int]("id").map(items - _))
    )
    closeAfterTest(cached.close())
    // Not get, as that fetches again if nothing is loaded yet
    eventually()(cached.tryModify(Some(_)))
    CachedItems(database, cached, fetches)

  test("loads the current data") {
    val database = setupDatabase()
    insert(database, 1, "a")
    val f = makeCached(database)
    assertEquals(f.cached.get, Map(1 -> "a"))
  }

  test("inserts are applied from notifications, without fetching again") {
    val f = makeCached(setupDatabase())
    insert(f.database, 1, "a")
    eventually()(f.cached.get == Map(1 -> "a"))
    assertEquals(f.fetches.get, 1)
  }

  test("updates are applied from notifications") {
    val database = setupDatabase()
    insert(database, 1, "a")
    val f = makeCached(database)
    database.executeSql("UPDATE items SET id = 2, name = 'b' WHERE id = 1")
    eventually()(f.cached.get == Map(2 -> "b"))
    assertEquals(f.fetches.get, 1)
  }

  test("deletes are applied from notifications") {
    val database = setupDatabase()
    insert(database, 1, "a")
    insert(database, 2, "b")
    val f = makeCached(database)
    database.executeSql("DELETE FROM items WHERE id = 1")
    eventually()(f.cached.get == Map(2 -> "b"))
    assertEquals(f.fetches.get, 1)
  }

  test("a notification whose payload can't be decoded warns, and fetches everything instead") {
    val f = makeCached(setupDatabase())
    // Sneak a row in without a notification, so we can see the fetch happening
    f.database.executeSql("ALTER TABLE items DISABLE TRIGGER items_notify")
    insert(f.database, 1, "a")
    f.database.notify(channel, """{"action": "INSERT", "payload": {"id": "not a number"}}""")
    eventually()(f.cached.get == Map(1 -> "a"))
    assertEquals(f.fetches.get, 2)
    assert(logged(Level.WARN).exists(w => w.contains("INSERT") && w.contains(channel)), logged(Level.WARN))
  }

  test("an operation without a handler fetches everything instead, without warning") {
    val database = setupDatabase()
    val f        = makeCached(database, withHandlers = false)
    insert(database, 1, "a")
    eventually()(f.cached.get == Map(1 -> "a"))
    assertEquals(f.fetches.get, 2)
    assertEquals(logged(Level.WARN), Nil)
  }

  test("notifications that can't be parsed warn, and fetch everything instead") {
    for (notification, i) <- Seq("not json", """{"foo": 1}""", """{"action": "INSERT"}""").zipWithIndex do
      val f = makeCached(setupDatabase(), s"Test$i")
      f.database.executeSql("ALTER TABLE items DISABLE TRIGGER items_notify")
      insert(f.database, 1, "a")
      f.database.notify(channel, notification)

      eventually()(f.cached.get == Map(1 -> "a"))
      assertEquals(f.fetches.get, 2, notification)
      assert(
        logged(Level.WARN).exists(w => w.contains(channel) && w.contains(notification)),
        s"No warning for $notification: ${logged(Level.WARN)}"
      )
  }

  test("notifications that can't be parsed don't stop later ones from working") {
    val f = makeCached(setupDatabase())
    f.database.notify(channel, "not json")
    insert(f.database, 1, "a")
    eventually()(f.cached.get == Map(1 -> "a"))
  }

  test("all instances listening get the changes") {
    val database = setupDatabase()
    val a        = makeCached(database, "A")
    val b        = makeCached(database, "B")
    insert(database, 1, "a")
    eventually()(a.cached.get == Map(1 -> "a") && b.cached.get == Map(1 -> "a"))
  }

  test("changes stop being applied after closing") {
    val f = makeCached(setupDatabase())
    f.cached.close()
    insert(f.database, 1, "a")
    Thread.sleep(500)
    server.getScheduler.performTicks(20)
    assertEquals(f.cached.get, Map.empty[Int, String])
  }

  test("invalid channel names are rejected") {
    given plugin: ScalaDbPlugin = createDbPlugin()
    val connection              = connect(setupDatabase(), plugin)
    for invalid <- Seq("HomeSweetHome.RequestsChange", "MixedCase", "with space", "1starts_with_digit", "") do
      intercept[IllegalArgumentException] {
        PostgresCached.postgresNotify[String](() => FutureOrNow.now(""), 1.hour, invalid, connection.pool)
      }
  }
