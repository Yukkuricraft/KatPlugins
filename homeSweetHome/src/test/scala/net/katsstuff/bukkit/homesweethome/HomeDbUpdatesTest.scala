package net.katsstuff.bukkit.homesweethome

import net.katsstuff.bukkit.homesweethome.testing.HomeSuite
import net.katsstuff.bukkit.katlib.db.testing.TestPostgres
import net.katsstuff.bukkit.katlib.db.testing.TestPostgres.TestDatabase
import skunk.codec.all.*
import skunk.implicits.*

class HomeDbUpdatesTest extends HomeSuite:

  private val tables = Set("version", "homes", "home_residents", "online_players", "requests", "invites", "delayed_teleports")

  private val channels = Map(
    "online_players_notify"    -> "homesweethome_global_players_change",
    "requests_notify"          -> "homesweethome_requests_change",
    "invites_notify"           -> "homesweethome_invites_change",
    "delayed_teleports_notify" -> "homesweethome_delayed_teleport_change"
  )

  /** The channel each trigger notifies. */
  private def triggerChannels(database: TestDatabase): Map[String, String] =
    database
      .query(
        sql"""SELECT t.tgname::text, encode(t.tgargs, 'escape')
              FROM pg_trigger t JOIN pg_class c ON c.oid = t.tgrelid JOIN pg_namespace n ON n.oid = c.relnamespace
              WHERE NOT t.tgisinternal AND n.nspname = current_schema()""".query(text ~ text)
      )
      .map((name, args) => name -> args.stripSuffix("\\000"))
      .toMap

  test("a fresh database gets all tables in the plugin's schema") {
    val database = migratedDatabase()
    assertEquals(database.tables, tables)
    assertEquals(database.version, Some(presentDbVersion.toShort))
  }

  test("all triggers notify on channels that can be listened to") {
    assertEquals(triggerChannels(migratedDatabase()), channels)
  }

  test("updating from the first version fixes the channels") {
    val database = TestPostgres.freshDatabase(schema = "homesweethome", createSchema = false)
    migrate(database, 1)
    assertEquals(triggerChannels(database)("requests_notify"), "HomeSweetHome.RequestsChange")

    migrate(database)
    assertEquals(triggerChannels(database), channels)
    assertEquals(database.version, Some(presentDbVersion.toShort))
  }

  test("updating again does nothing") {
    val database = migratedDatabase()
    migrate(database)
    assertEquals(database.tables, tables)
  }

  test("home changes send notifications with the column names") {
    val database = migratedDatabase()
    val listener = database.listen("homesweethome_requests_change")
    closeAfterTest(listener.close())
    database.executeSql(
      "INSERT INTO requests VALUES ('00000000-0000-0000-0000-000000000001', '00000000-0000-0000-0000-000000000002', '{}', now())"
    )
    eventually()(listener.messages.nonEmpty)
    assert(listener.messages.head.contains("\"home_owner\""), listener.messages.head)
  }

  test("residents are removed with their home") {
    val database = migratedDatabase()
    database.executeSql(
      """INSERT INTO homes VALUES ('00000000-0000-0000-0000-000000000001', 'home', now(), now(), 0, 0, 0, 0, 0, '00000000-0000-0000-0000-000000000003', 'a');
        |INSERT INTO home_residents VALUES ('00000000-0000-0000-0000-000000000001', 'home', '00000000-0000-0000-0000-000000000002', now());
        |DELETE FROM homes""".stripMargin
    )
    assertEquals(database.query(sql"SELECT count(*) FROM home_residents".query(int8)), List(0L))
  }
