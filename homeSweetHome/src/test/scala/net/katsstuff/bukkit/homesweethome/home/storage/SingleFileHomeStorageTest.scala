package net.katsstuff.bukkit.homesweethome.home.storage

import java.nio.file.Files
import java.util.UUID

import be.seeseemelk.mockbukkit.WorldMock
import net.katsstuff.bukkit.homesweethome.home.Home
import net.katsstuff.bukkit.homesweethome.testing.HomeSuite
import net.katsstuff.bukkit.homesweethome.{HSHConfig, HomePlugin}
import org.bukkit.Location

class SingleFileHomeStorageTest extends HomeSuite:

  private val path = Files.createTempDirectory("homes").resolve("storage.json")

  private def storage(): SingleFileHomeStorage =
    given HomePlugin = createHomePlugin()
    given HSHConfig  = hshConfig()
    val s            = new SingleFileHomeStorage(path)
    await(s.reloadHomeData())
    s

  override def beforeEach(context: BeforeEach): Unit =
    super.beforeEach(context)
    Files.deleteIfExists(path)

  private def location(world: WorldMock, x: Double): Location = new Location(world, x, 64, 0, 45, 10)

  private def names(homes: Seq[Home]): Seq[String] = homes.map(_.name)

  test("homes and residents are saved and loaded again") {
    val s        = storage()
    val world    = server.addSimpleWorld("world")
    val owner    = UUID.randomUUID()
    val resident = UUID.randomUUID()
    result(s.makeHome(owner, "home", location(world, 10)))
    result(s.makeHome(owner, "other", location(world, 20)))
    result(s.addResident(owner, "home", resident))

    val loaded = storage()
    assertEquals(result(loaded.allHomesForPlayer(owner)), result(s.allHomesForPlayer(owner)))
    assertEquals(result(loaded.getHomeResidents(owner, "home")), Set(resident))
    assert(result(loaded.isPlayerResident(owner, "home", resident)))
    assert(!result(loaded.isPlayerResident(owner, "other", resident)))
  }

  test("deleting a home removes its residents") {
    val s        = storage()
    val world    = server.addSimpleWorld("world")
    val owner    = UUID.randomUUID()
    val resident = UUID.randomUUID()
    result(s.makeHome(owner, "home", location(world, 10)))
    result(s.addResident(owner, "home", resident))
    result(s.deleteHome(owner, "home"))

    val loaded = storage()
    assert(!result(loaded.homeExist(owner, "home")))
    assertEquals(result(loaded.getHomeResidents(owner, "home")), Set.empty[UUID])
  }

  test("residents can't be added to homes that don't exist") {
    val s = storage()
    intercept[Exception](result(s.addResident(UUID.randomUUID(), "home", UUID.randomUUID())))
  }

  test("searching homes filters and sorts by distance") {
    val s      = storage()
    val world  = server.addSimpleWorld("world")
    val nether = server.addSimpleWorld("nether")
    val owner  = UUID.randomUUID()
    val other  = UUID.randomUUID()
    result(s.makeHome(owner, "far", location(world, 100)))
    result(s.makeHome(owner, "near", location(world, 5)))
    result(s.makeHome(owner, "middle", location(world, 50)))
    result(s.makeHome(owner, "nether", location(nether, 1)))
    result(s.makeHome(other, "others", location(world, 2)))

    val origin = location(world, 0)
    assertEquals(names(result(s.searchHomes(origin))), Seq("nether", "others", "near", "middle", "far"))
    assertEquals(names(result(s.searchHomes(origin, radius = Some(60)))), Seq("nether", "others", "near", "middle"))
    assertEquals(names(result(s.searchHomes(origin, world = Some(world.getUID)))), Seq("others", "near", "middle", "far"))
    assertEquals(names(result(s.searchHomes(origin, owner = Some(other)))), Seq("others"))
    assertEquals(
      names(result(s.searchHomes(origin, world = Some(world.getUID), owner = Some(owner), drop = 1, take = 1))),
      Seq("middle")
    )
    assertEquals(names(result(s.searchHomes(origin, drop = 3))), Seq("middle", "far"))
    assertEquals(names(result(s.searchHomes(origin, take = 0))), Seq())
  }

  test("exported data can be imported again") {
    val s        = storage()
    val world    = server.addSimpleWorld("world")
    val owner    = UUID.randomUUID()
    val resident = UUID.randomUUID()
    result(s.makeHome(owner, "home", location(world, 10)))
    result(s.addResident(owner, "home", resident))
    val (homes, residents) = result(s.exportData())

    Files.deleteIfExists(path)
    val fresh = storage()
    result(fresh.makeHome(UUID.randomUUID(), "replaced", location(world, 10)))
    result(fresh.importData(homes, residents))

    val loaded = storage()
    assertEquals(loaded.homeOwners, Set(owner))
    assertEquals(result(loaded.getHomeResidents(owner, "home")), Set(resident))
  }
