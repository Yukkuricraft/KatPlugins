package net.katsstuff.bukkit.magicalwarps.warp.storage

import net.katsstuff.bukkit.magicalwarps.testing.WarpsSuite
import net.katsstuff.bukkit.magicalwarps.warp.Warp
import skunk.codec.all.*
import skunk.implicits.*

class PostgresWarpStorageTest extends WarpsSuite:

  test("warps can be set and read back") {
    val a     = startWarpServer(migratedDatabase())
    val spawn = fullWarp("spawn")
    result(a.setWarp(spawn))
    assertEquals(a.allWarps, Map("spawn" -> spawn))
  }

  test("warps are saved") {
    val database = migratedDatabase()
    val spawn    = fullWarp("spawn")
    result(startWarpServer(database).setWarp(spawn))
    assertEquals(startWarpServer(database, "b").allWarps, Map("spawn" -> spawn))
  }

  test("setting a warp that exists updates it") {
    val database = migratedDatabase()
    val a        = startWarpServer(database)
    result(a.setWarp(warp("spawn", x = 1)))
    result(a.setWarp(warp("spawn", x = 2)))
    assertEquals(a.allWarps.get("spawn").map(_.x), Some(2.0))
    assertEquals(database.query(sql"SELECT count(*) FROM warps".query(int8)), List(1L))
  }

  test("warp names keep their case") {
    val a = startWarpServer(migratedDatabase())
    result(a.setWarp(warp("Spawn")))
    result(a.setWarp(warp("spawn")))
    assertEquals(a.allWarps.keySet, Set("Spawn", "spawn"))
    result(a.removeWarp("spawn"))
    assertEquals(a.getWarp("Spawn").map(_.name), Some("Spawn"))
    assertEquals(a.getWarp("spawn"), None)
  }

  test("warps can be removed") {
    val database = migratedDatabase()
    val a        = startWarpServer(database)
    result(a.setWarp(warp("spawn")))
    result(a.setWarp(warp("other")))
    result(a.removeWarp("spawn"))
    assertEquals(a.allWarps.keySet, Set("other"))
    assertEquals(startWarpServer(database, "b").allWarps.keySet, Set("other"))
  }

  test("warps set on one server are seen on the others") {
    val database = migratedDatabase()
    val a        = startWarpServer(database)
    val b        = startWarpServer(database, "b")
    val spawn    = fullWarp("spawn")

    result(a.setWarp(spawn))
    eventually()(b.allWarps == Map("spawn" -> spawn))

    val moved = spawn.copy(x = 500)
    result(a.setWarp(moved))
    eventually()(b.allWarps == Map("spawn" -> moved))

    result(a.removeWarp("spawn"))
    eventually()(b.allWarps.isEmpty)
  }

  test("renaming a warp moves it to the new name") {
    val database = migratedDatabase()
    val a        = startWarpServer(database)
    val b        = startWarpServer(database, "b")
    val spawn    = fullWarp("spawn")
    result(a.setWarp(spawn))

    result(a.renameWarp(spawn, "hub"))
    val hub: Warp = spawn.copy(name = "hub")
    assertEquals(a.allWarps, Map("hub" -> hub))
    eventually()(b.allWarps == Map("hub" -> hub))
    assertEquals(database.query(sql"SELECT name FROM warps".query(text)), List("hub"))
  }

  test("renaming a warp to a name that is taken fails, and changes nothing") {
    val database = migratedDatabase()
    val a        = startWarpServer(database)
    val spawn    = warp("spawn", x = 1)
    val hub      = warp("hub", x = 2)
    result(a.setWarp(spawn))
    result(a.setWarp(hub))

    intercept[Exception](result(a.renameWarp(spawn, "hub")))
    assertEquals(a.allWarps, Map("spawn" -> spawn, "hub" -> hub))
    assertEquals(startWarpServer(database, "b").allWarps, Map("spawn" -> spawn, "hub" -> hub))
  }

  test("reloading picks up changes that were missed") {
    val database = migratedDatabase()
    val a        = startWarpServer(database)
    database.executeSql("ALTER TABLE warps DISABLE TRIGGER warps_notify")
    database.executeSql(
      "INSERT INTO warps VALUES ('quiet', 0, 0, 0, 0, 0, '00000000-0000-0000-0000-000000000001', 'a', NULL, '{}', '{}', '{}', NULL)"
    )
    assertEquals(a.allWarps, Map.empty[String, Warp])
    await(a.reloadWarps())
    assertEquals(a.allWarps.keySet, Set("quiet"))
  }

  test("groups come from the warps") {
    val a = startWarpServer(migratedDatabase())
    result(a.setWarp(warp("spawn").copy(groups = Seq("hubs"))))
    result(a.setWarp(warp("town").copy(groups = Seq("hubs", "towns"))))
    result(a.setWarp(warp("lonely")))

    assertEquals(a.groups.toSet, Set("hubs", "towns", "all"))
    assertEquals(a.getGroupWarps("hubs").keySet, Set("spawn", "town"))
    assertEquals(a.getGroupWarps("towns").keySet, Set("town"))
    assertEquals(a.getGroupWarps("all").keySet, Set("spawn", "town", "lonely"))
  }

  test("exported warps can be imported, replacing what was there") {
    val database = migratedDatabase()
    val a        = startWarpServer(database)
    val warps    = Seq(fullWarp("spawn"), warp("hub"))
    warps.foreach(w => result(a.setWarp(w)))
    assertEquals(result(a.exportData()).toSet, warps.toSet)

    val otherDatabase = migratedDatabase()
    val c             = startWarpServer(otherDatabase, "c")
    val d             = startWarpServer(otherDatabase, "d")
    result(c.setWarp(warp("replaced")))
    result(c.importData(result(a.exportData())))

    val expected = warps.map(w => w.name -> w).toMap
    assertEquals(c.allWarps, expected)
    eventually()(d.allWarps == expected)
  }

  test("importing nothing removes everything") {
    val a = startWarpServer(migratedDatabase())
    result(a.setWarp(warp("spawn")))
    result(a.importData(Nil))
    assertEquals(a.allWarps, Map.empty[String, Warp])
  }
