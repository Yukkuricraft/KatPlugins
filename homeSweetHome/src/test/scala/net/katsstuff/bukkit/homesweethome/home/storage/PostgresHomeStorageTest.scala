package net.katsstuff.bukkit.homesweethome.home.storage

import java.util.UUID

import be.seeseemelk.mockbukkit.WorldMock
import be.seeseemelk.mockbukkit.entity.PlayerMock
import net.katsstuff.bukkit.homesweethome.home.Home
import net.katsstuff.bukkit.homesweethome.testing.HomeSuite
import org.bukkit.Location

class PostgresHomeStorageTest extends HomeSuite:

  private def location(world: WorldMock, x: Double, y: Double = 64, z: Double = 0): Location =
    new Location(world, x, y, z, 45, 10)

  private def names(homes: Seq[Home]): Seq[String] = homes.map(_.name)

  test("homes can be made and read back for players that aren't online") {
    val a     = startHomeServer(migratedDatabase())
    val world = server.addSimpleWorld("world")
    val owner = UUID.randomUUID()

    result(a.storage.makeHome(owner, "home", location(world, 10)))
    result(a.storage.makeHome(owner, "other", location(world, 20)))

    val home = result(a.storage.specificHome(owner, "home")).get
    assertEquals((home.owner, home.name, home.x, home.yaw, home.worldUuid, home.server), (owner, "home", 10.0, 45f, world.getUID, "a"))
    assertEquals(result(a.storage.allHomesForPlayer(owner)).keySet, Set("home", "other"))
    assertEquals(result(a.storage.homeCount(owner)), 2)
    assert(result(a.storage.homeExist(owner, "home")))
    assert(!result(a.storage.homeExist(owner, "missing")))
    assertEquals(result(a.storage.specificHome(owner, "missing")), None)
  }

  test("a home read back from the database is the same as the one in memory") {
    val database = migratedDatabase()
    val a        = startHomeServer(database)
    val world    = server.addSimpleWorld("world")
    val owner    = server.addPlayer()

    result(a.storage.makeHome(owner.getUniqueId, "home", location(world, 10)))
    val inMemory = a.storage.specificHome(owner.getUniqueId, "home")
    assert(inMemory.isNow)

    // Started after the player joined, so it has to go to the database
    val b      = startHomeServer(database, "b")
    val fromDb = b.storage.specificHome(owner.getUniqueId, "home")
    assert(!fromDb.isNow)
    assertEquals(result(fromDb), result(inMemory))
  }

  test("lookups for players that aren't online don't mix up homes and residents") {
    val a        = startHomeServer(migratedDatabase())
    val world    = server.addSimpleWorld("world")
    val owner    = UUID.randomUUID()
    val resident = UUID.randomUUID()
    result(a.storage.makeHome(owner, "home", location(world, 10)))
    result(a.storage.makeHome(owner, "other", location(world, 20)))
    result(a.storage.addResident(owner, "home", resident))

    // Asked right after each other, so any caching kicks in
    assert(result(a.storage.homeExist(owner, "home")))
    assert(!result(a.storage.homeExist(owner, "missing")))
    assertEquals(result(a.storage.specificHome(owner, "home")).map(_.x), Some(10.0))
    assertEquals(result(a.storage.specificHome(owner, "other")).map(_.x), Some(20.0))
    assertEquals(result(a.storage.getHomeResidents(owner, "home")), Set(resident))
    assertEquals(result(a.storage.getHomeResidents(owner, "other")), Set.empty[UUID])
    assert(result(a.storage.isPlayerResident(owner, "home", resident)))
    assert(!result(a.storage.isPlayerResident(owner, "home", UUID.randomUUID())))
  }

  test("changes to homes of players that aren't online are seen right away") {
    val a        = startHomeServer(migratedDatabase())
    val world    = server.addSimpleWorld("world")
    val owner    = UUID.randomUUID()
    val resident = UUID.randomUUID()

    assert(!result(a.storage.homeExist(owner, "home")))
    assertEquals(result(a.storage.homeCount(owner)), 0)
    result(a.storage.makeHome(owner, "home", location(world, 10)))
    assert(result(a.storage.homeExist(owner, "home")))
    assertEquals(result(a.storage.homeCount(owner)), 1)
    assertEquals(result(a.storage.allHomesForPlayer(owner)).keySet, Set("home"))

    assert(!result(a.storage.isPlayerResident(owner, "home", resident)))
    result(a.storage.addResident(owner, "home", resident))
    assert(result(a.storage.isPlayerResident(owner, "home", resident)))
    assertEquals(result(a.storage.allResidentsForPlayer(owner)), Map("home" -> Set(resident)))
    result(a.storage.removeResident(owner, "home", resident))
    assert(!result(a.storage.isPlayerResident(owner, "home", resident)))

    result(a.storage.deleteHome(owner, "home"))
    assert(!result(a.storage.homeExist(owner, "home")))
    assertEquals(result(a.storage.homeCount(owner)), 0)
  }

  test("making a home with an existing name replaces it") {
    val a     = startHomeServer(migratedDatabase())
    val world = server.addSimpleWorld("world")
    val owner = UUID.randomUUID()

    result(a.storage.makeHome(owner, "home", location(world, 10)))
    result(a.storage.makeHome(owner, "home", location(world, 30)))
    assertEquals(result(a.storage.homeCount(owner)), 1)
    assertEquals(result(a.storage.specificHome(owner, "home")).map(_.x), Some(30.0))
  }

  test("deleting a home removes it and its residents") {
    val a        = startHomeServer(migratedDatabase())
    val world    = server.addSimpleWorld("world")
    val owner    = UUID.randomUUID()
    val resident = UUID.randomUUID()

    result(a.storage.makeHome(owner, "home", location(world, 10)))
    result(a.storage.addResident(owner, "home", resident))
    result(a.storage.deleteHome(owner, "home"))

    assert(!result(a.storage.homeExist(owner, "home")))
    // Making the home again doesn't bring the residents back
    result(a.storage.makeHome(owner, "home", location(world, 10)))
    assertEquals(result(a.storage.getHomeResidents(owner, "home")), Set.empty[UUID])
  }

  test("homes of online players are served from memory, and still saved") {
    val database = migratedDatabase()
    val a        = startHomeServer(database)
    val world    = server.addSimpleWorld("world")
    val owner    = server.addPlayer()

    result(a.storage.makeHome(owner.getUniqueId, "home", location(world, 10)))
    val lookup = a.storage.specificHome(owner.getUniqueId, "home")
    assert(lookup.isNow)
    assertEquals(result(lookup).map(_.x), Some(10.0))

    val b = startHomeServer(database, "b")
    assertEquals(result(b.storage.specificHome(owner.getUniqueId, "home")).map(_.x), Some(10.0))
  }

  test("joining loads the player's homes and residents") {
    val database = migratedDatabase()
    val a        = startHomeServer(database)
    val world    = server.addSimpleWorld("world")
    val ownerId  = UUID.randomUUID()
    val resident = UUID.randomUUID()
    result(a.storage.makeHome(ownerId, "home", location(world, 10)))
    result(a.storage.addResident(ownerId, "home", resident))

    server.addPlayer(new PlayerMock(server, "Owner", ownerId))

    eventuallyAssert() {
      val homes = a.storage.allHomesForPlayer(ownerId)
      assert(homes.isNow)
      assertEquals(result(homes).keySet, Set("home"))
    }
    eventuallyAssert()(assertEquals(result(a.storage.getHomeResidents(ownerId, "home")), Set(resident)))
    assert(a.storage.getHomeResidents(ownerId, "home").isNow)
  }

  test("leaving forgets the player's homes and residents") {
    val a        = startHomeServer(migratedDatabase())
    val world    = server.addSimpleWorld("world")
    val owner    = server.addPlayer()
    val resident = UUID.randomUUID()
    result(a.storage.makeHome(owner.getUniqueId, "home", location(world, 10)))
    result(a.storage.addResident(owner.getUniqueId, "home", resident))
    eventually()(a.storage.getHomeResidents(owner.getUniqueId, "home").isNow)

    owner.disconnect()

    assert(!a.storage.allHomesForPlayer(owner.getUniqueId).isNow)
    assert(!a.storage.getHomeResidents(owner.getUniqueId, "home").isNow)
    // Still in the database
    assertEquals(result(a.storage.getHomeResidents(owner.getUniqueId, "home")), Set(resident))
  }

  test("reloading loads the homes of online players") {
    val database = migratedDatabase()
    val world    = server.addSimpleWorld("world")
    val owner    = server.addPlayer()
    val a        = startHomeServer(database)
    result(startHomeServer(database, "b").storage.makeHome(owner.getUniqueId, "fromB", location(world, 10)))

    await(a.storage.reloadHomeData())
    val homes = a.storage.allHomesForPlayer(owner.getUniqueId)
    assert(homes.isNow)
    assertEquals(result(homes).keySet, Set("fromB"))
  }

  test("removing a resident only removes that resident") {
    for online <- Seq(false, true) do
      val a        = startHomeServer(migratedDatabase(), s"server-$online")
      val world    = server.addSimpleWorld(s"world-$online")
      val owner    = if online then server.addPlayer().getUniqueId else UUID.randomUUID()
      val resident = UUID.randomUUID()
      val other    = UUID.randomUUID()
      result(a.storage.makeHome(owner, "home", location(world, 10)))
      result(a.storage.addResident(owner, "home", resident))
      result(a.storage.addResident(owner, "home", other))

      result(a.storage.removeResident(owner, "home", resident))
      assertEquals(result(a.storage.getHomeResidents(owner, "home")), Set(other), s"online = $online")
      assertEquals(result(a.storage.exportData())._2.map(_.resident), Seq(other), s"online = $online")
  }

  test("only residents are residents") {
    val a        = startHomeServer(migratedDatabase())
    val world    = server.addSimpleWorld("world")
    val owner    = UUID.randomUUID()
    val resident = UUID.randomUUID()
    result(a.storage.makeHome(owner, "home", location(world, 10)))
    result(a.storage.makeHome(owner, "other", location(world, 10)))
    result(a.storage.addResident(owner, "home", resident))

    assert(result(a.storage.isPlayerResident(owner, "home", resident)))
    assert(!result(a.storage.isPlayerResident(owner, "home", UUID.randomUUID())))
    assert(!result(a.storage.isPlayerResident(owner, "other", resident)))
  }

  test("residents are grouped by home") {
    val a     = startHomeServer(migratedDatabase())
    val world = server.addSimpleWorld("world")
    val owner = UUID.randomUUID()
    val (r1, r2, r3) = (UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID())
    result(a.storage.makeHome(owner, "home", location(world, 10)))
    result(a.storage.makeHome(owner, "other", location(world, 10)))
    result(a.storage.makeHome(owner, "empty", location(world, 10)))
    result(a.storage.addResident(owner, "home", r1))
    result(a.storage.addResident(owner, "home", r2))
    result(a.storage.addResident(owner, "other", r3))

    assertEquals(result(a.storage.allResidentsForPlayer(owner)), Map("home" -> Set(r1, r2), "other" -> Set(r3)))
  }

  test("searching homes filters and sorts by distance") {
    val a      = startHomeServer(migratedDatabase())
    val world  = server.addSimpleWorld("world")
    val nether = server.addSimpleWorld("nether")
    val owner  = UUID.randomUUID()
    val other  = UUID.randomUUID()
    result(a.storage.makeHome(owner, "far", location(world, 100)))
    result(a.storage.makeHome(owner, "near", location(world, 5)))
    result(a.storage.makeHome(owner, "middle", location(world, 50)))
    result(a.storage.makeHome(owner, "nether", location(nether, 1)))
    result(a.storage.makeHome(other, "others", location(world, 2)))

    val origin = location(world, 0)
    assertEquals(names(result(a.storage.searchHomes(origin))), Seq("nether", "others", "near", "middle", "far"))
    assertEquals(names(result(a.storage.searchHomes(origin, radius = Some(60)))), Seq("nether", "others", "near", "middle"))
    assertEquals(names(result(a.storage.searchHomes(origin, world = Some(world.getUID)))), Seq("others", "near", "middle", "far"))
    assertEquals(names(result(a.storage.searchHomes(origin, owner = Some(other)))), Seq("others"))
    assertEquals(
      names(result(a.storage.searchHomes(origin, world = Some(world.getUID), owner = Some(owner), drop = 1, take = 1))),
      Seq("middle")
    )
    assertEquals(names(result(a.storage.searchHomes(origin, drop = 3))), Seq("middle", "far"))
    assertEquals(names(result(a.storage.searchHomes(origin, take = 0))), Seq())
  }

  test("home owners are known when starting") {
    val database = migratedDatabase()
    val world    = server.addSimpleWorld("world")
    val player   = server.addPlayer()
    val offline  = UUID.randomUUID()
    val setup    = startHomeServer(database, "setup")
    result(setup.storage.makeHome(player.getUniqueId, "home", location(world, 10)))
    result(setup.storage.makeHome(offline, "home", location(world, 10)))

    val a = startHomeServer(database)
    eventually()(a.storage.homeOwners == Set(player.getUniqueId, offline))
    eventuallyAssert()(assertEquals(a.storage.homeOwnersPlayers.get(player.getName).map(_.getUniqueId), Some(player.getUniqueId)))
  }

  test("exported data can be imported into another database") {
    val a     = startHomeServer(migratedDatabase())
    val world = server.addSimpleWorld("world")
    val owner = UUID.randomUUID()
    val res   = UUID.randomUUID()
    result(a.storage.makeHome(owner, "home", location(world, 10)))
    result(a.storage.makeHome(owner, "other", location(world, 20)))
    result(a.storage.addResident(owner, "home", res))

    val (homes, residents) = result(a.storage.exportData())
    assertEquals(homes.size, 2)
    assertEquals(residents.map(r => (r.owner, r.homeName, r.resident)), Seq((owner, "home", res)))

    val b     = startHomeServer(migratedDatabase(), "b")
    val bHome = UUID.randomUUID()
    result(b.storage.makeHome(bHome, "replaced", location(world, 10)))
    result(b.storage.importData(homes, residents))

    assertEquals(result(b.storage.allHomesForPlayer(owner)).keySet, Set("home", "other"))
    assertEquals(result(b.storage.getHomeResidents(owner, "home")), Set(res))
    assert(!result(b.storage.homeExist(bHome, "replaced")))
    val (bHomes, bResidents) = result(b.storage.exportData())
    assertEquals(bHomes.map(truncated).toSet, homes.map(truncated).toSet)
    assertEquals(bResidents.map(r => (r.owner, r.homeName, r.resident)), Seq((owner, "home", res)))
  }
