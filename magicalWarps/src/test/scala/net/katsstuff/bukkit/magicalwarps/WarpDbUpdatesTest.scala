package net.katsstuff.bukkit.magicalwarps

import io.circe.parser
import net.katsstuff.bukkit.magicalwarps.testing.WarpsSuite
import net.katsstuff.bukkit.magicalwarps.warp.Warp
import skunk.codec.all.*
import skunk.implicits.*

class WarpDbUpdatesTest extends WarpsSuite:

  test("a fresh database gets all tables in the plugin's schema") {
    val database = migratedDatabase()
    assertEquals(database.tables, Set("version", "warps", "delayed_teleports"))
    assertEquals(database.version, Some(presentDbVersion.toShort))
  }

  test("warp names are unique") {
    val database = migratedDatabase()
    val insert =
      "INSERT INTO warps VALUES ('spawn', 0, 0, 0, 0, 0, '00000000-0000-0000-0000-000000000001', 'a', NULL, '{}', '{}', '{}', NULL)"
    database.executeSql(insert)
    intercept[Exception](database.executeSql(insert))
  }

  test("warps can have several allowed users") {
    val database = migratedDatabase()
    database.executeSql(
      """INSERT INTO warps VALUES ('spawn', 0, 0, 0, 0, 0, '00000000-0000-0000-0000-000000000001', 'a', NULL, '{}', '{}',
        |'{00000000-0000-0000-0000-000000000002, 00000000-0000-0000-0000-000000000003}', NULL)""".stripMargin
    )
    assertEquals(database.query(sql"SELECT cardinality(allowed_users) FROM warps".query(int4)), List(2))
  }

  test("warp changes are sent on a channel that can be listened to, as warps") {
    val database = migratedDatabase()
    val listener = database.listen("magicalwarps_warps_change")
    closeAfterTest(listener.close())
    val storage = startWarpServer(database)
    val saved   = fullWarp("spawn")
    result(storage.setWarp(saved))

    eventually()(listener.messages.nonEmpty)
    val payload = parser.parse(listener.messages.head).toTry.get.hcursor
    assertEquals(payload.get[String]("action"), Right("INSERT"))
    assertEquals(payload.get[Warp]("payload"), Right(saved))
  }
