package net.katsstuff.bukkit.katlib.db

import java.util.UUID

import scala.concurrent.{ExecutionContext, Future}

import be.seeseemelk.mockbukkit.WorldMock
import be.seeseemelk.mockbukkit.entity.PlayerMock
import dataprism.sql.Db
import net.katsstuff.bukkit.katlib.db.testing.DbSuite
import net.katsstuff.bukkit.katlib.db.testing.TestPostgres.TestDatabase
import net.katsstuff.bukkit.katlib.testing.RecordingPlayerMock
import net.katsstuff.bukkit.katlib.{BungeeChannel, GlobalPlayer}
import org.bukkit.{Bukkit, Location}
import skunk.Codec
import skunk.codec.all.*
import skunk.implicits.*

/**
  * Tests for the teleporter, run by each plugin against its own database
  * updates.
  */
abstract class CrossServerPostgresTeleporterSuite extends DbSuite:

  /** The channel the plugin uses for delayed teleports. */
  def channel: String

  /** A fresh database with all the plugin's updates applied. */
  def migratedDatabase(): TestDatabase

  case class Server(name: String, plugin: ScalaDbPlugin, teleporter: CrossServerPostgresTeleporter)

  /** Starts a teleporter as if on the given server. All share one mocked server. */
  def startServer(database: TestDatabase, name: String): Server =
    given plugin: ScalaDbPlugin = createDbPlugin(name)
    // What a plugin needs to talk to the proxy
    Bukkit.getMessenger.registerOutgoingPluginChannel(plugin, "BungeeCord")
    val connection          = connect(database, plugin)
    given Db[Future, Codec] = connection.db
    given BungeeChannel     = new BungeeChannel
    val teleporter          = new CrossServerPostgresTeleporter(connection.pool, name, channel)
    Bukkit.getPluginManager.registerEvents(teleporter, plugin)
    closeAfterTest(teleporter.close())
    Server(name, plugin, teleporter)

  def delayedTeleports(database: TestDatabase): List[(UUID, String, Double)] =
    database.query(sql"SELECT uuid, server, x FROM delayed_teleports ORDER BY server".query(uuid ~ text ~ float8)).map {
      case ((u, s), x) => (u, s, x)
    }

  def insertDelayedTeleport(
      database: TestDatabase,
      player: UUID,
      server: String,
      world: UUID,
      x: Double,
      expires: String = "now() + interval '1 minute'"
  ): Unit =
    database.executeSql(
      s"INSERT INTO delayed_teleports VALUES ('$player', $x, 64, 20, 90, 10, '$world', '$server', $expires)"
    )

  /** Saves a teleport without notifying anyone, so only joining or starting can find it. */
  def insertDelayedTeleportQuietly(
      database: TestDatabase,
      player: UUID,
      server: String,
      world: UUID,
      x: Double,
      expires: String = "now() + interval '1 minute'"
  ): Unit =
    database.executeSql("ALTER TABLE delayed_teleports DISABLE TRIGGER delayed_teleports_notify")
    insertDelayedTeleport(database, player, server, world, x, expires)
    database.executeSql("ALTER TABLE delayed_teleports ENABLE TRIGGER delayed_teleports_notify")

  def assertAt(player: PlayerMock, world: WorldMock, x: Double): Unit =
    val location = player.getLocation
    assertEquals(location.getWorld, world)
    assertEqualsDouble(location.getX, x, 0.001)
    assertEqualsDouble(location.getY, 64, 0.001)
    assertEqualsDouble(location.getZ, 20, 0.001)

  def destination(x: Double): Location = new Location(null, x, 64, 20, 90, 10)

  test("the database updates create the delayed teleport table and trigger") {
    val database = migratedDatabase()
    assert(database.tables.contains("delayed_teleports"))
    val listener = database.listen(channel)
    closeAfterTest(listener.close())
    insertDelayedTeleport(database, UUID.randomUUID(), "b", UUID.randomUUID(), 1)
    eventually()(listener.messages.nonEmpty)
    assert(listener.messages.head.contains("\"INSERT\""), listener.messages.head)
  }

  test("teleports within the same server happen right away") {
    val database = migratedDatabase()
    val a        = startServer(database, "a")
    val world    = server.addSimpleWorld("world")
    val player   = server.addPlayer()

    assertEquals(a.teleporter.teleport(GlobalPlayer.OnThisServer(player), destination(100), world.getUID, "a"), Right(()))
    assertAt(player, world, 100)
    assertEquals(delayedTeleports(database), Nil)
  }

  test("teleports to a world that doesn't exist fail") {
    val database = migratedDatabase()
    val a        = startServer(database, "a")
    val player   = server.addPlayer()
    assert(a.teleporter.teleport(GlobalPlayer.OnThisServer(player), destination(100), UUID.randomUUID(), "a").isLeft)
  }

  test("teleports to another server are saved, and the player is sent there") {
    val database = migratedDatabase()
    val a        = startServer(database, "a")
    val world    = server.addSimpleWorld("world")
    val player   = new RecordingPlayerMock(server, "Traveller")
    server.addPlayer(player)

    assertEquals(a.teleporter.teleport(GlobalPlayer.OnThisServer(player), destination(100), world.getUID, "b"), Right(()))
    eventually()(delayedTeleports(database) == List((player.getUniqueId, "b", 100.0)))
    assertEquals(player.pluginMessages.map(_._2), Seq("BungeeCord"))
    assert(new String(player.pluginMessages.head._3, "UTF-8").contains("Connect"))
  }

  test("the other server teleports the player when the teleport is saved") {
    val database = migratedDatabase()
    val a        = startServer(database, "a")
    startServer(database, "b")
    val world  = server.addSimpleWorld("world")
    val player = server.addPlayer()

    a.teleporter.teleport(GlobalPlayer.OnThisServer(player), destination(100), world.getUID, "b")

    // Everything runs in one mocked server, so the player is already "on" server b
    eventuallyAssert()(assertAt(player, world, 100))
    eventually()(delayedTeleports(database).isEmpty)
  }

  test("saving a new teleport for a player replaces the old one") {
    val database = migratedDatabase()
    val a        = startServer(database, "a")
    val world    = server.addSimpleWorld("world")
    val player   = server.addPlayer()

    a.teleporter.teleport(GlobalPlayer.OnThisServer(player), destination(100), world.getUID, "b")
    eventually()(delayedTeleports(database).size == 1)
    a.teleporter.teleport(GlobalPlayer.OnThisServer(player), destination(200), world.getUID, "c")
    eventually()(delayedTeleports(database) == List((player.getUniqueId, "c", 200.0)))
  }

  test("a player joining gets their saved teleport") {
    val database = migratedDatabase()
    startServer(database, "b")
    val world      = server.addSimpleWorld("world")
    val playerUuid = UUID.randomUUID()
    insertDelayedTeleportQuietly(database, playerUuid, "b", world.getUID, 100)

    val player = new PlayerMock(server, "Joining", playerUuid)
    server.addPlayer(player)

    eventuallyAssert()(assertAt(player, world, 100))
    eventually()(delayedTeleports(database).isEmpty)
  }

  test("a player joining doesn't get teleports saved for other servers") {
    val database = migratedDatabase()
    startServer(database, "b")
    val world      = server.addSimpleWorld("world")
    val playerUuid = UUID.randomUUID()
    insertDelayedTeleportQuietly(database, playerUuid, "c", world.getUID, 100)

    val player = new PlayerMock(server, "Joining", playerUuid)
    val before = player.getLocation
    server.addPlayer(player)
    Thread.sleep(300)
    server.getScheduler.performTicks(10)

    assertEquals(player.getLocation, before)
    assertEquals(delayedTeleports(database), List((playerUuid, "c", 100.0)))
  }

  test("expired teleports are ignored when joining") {
    val database = migratedDatabase()
    startServer(database, "b")
    val world      = server.addSimpleWorld("world")
    val playerUuid = UUID.randomUUID()
    insertDelayedTeleportQuietly(database, playerUuid, "b", world.getUID, 100, expires = "now() - interval '1 minute'")

    val player = new PlayerMock(server, "Joining", playerUuid)
    val before = player.getLocation
    server.addPlayer(player)
    Thread.sleep(300)
    server.getScheduler.performTicks(10)

    assertEquals(player.getLocation, before)
  }

  test("saved teleports for online players are done when the server starts") {
    val database = migratedDatabase()
    val world    = server.addSimpleWorld("world")
    val player   = server.addPlayer()
    insertDelayedTeleportQuietly(database, player.getUniqueId, "b", world.getUID, 100)

    startServer(database, "b")
    eventuallyAssert()(assertAt(player, world, 100))
    eventually()(delayedTeleports(database).isEmpty)
  }

  test("expired teleports are removed when the server starts") {
    val database = migratedDatabase()
    val world    = server.addSimpleWorld("world")
    insertDelayedTeleport(database, UUID.randomUUID(), "c", world.getUID, 100, expires = "now() - interval '1 minute'")
    insertDelayedTeleport(database, UUID.randomUUID(), "c", world.getUID, 200)

    startServer(database, "b")
    eventually()(delayedTeleports(database).map(_._3) == List(200.0))
  }

  test("players on other servers can be sent if someone is online here") {
    val database = migratedDatabase()
    val a        = startServer(database, "a")
    val world    = server.addSimpleWorld("world")
    server.addPlayer()
    val otherUuid = UUID.randomUUID()

    assertEquals(
      a.teleporter.teleport(GlobalPlayer.OnOtherServer("Other", otherUuid), destination(100), world.getUID, "b"),
      Right(())
    )
    eventually()(delayedTeleports(database) == List((otherUuid, "b", 100.0)))
  }

  test("players on other servers can't be sent if nobody is online here") {
    val database = migratedDatabase()
    val a        = startServer(database, "a")
    val world    = server.addSimpleWorld("world")
    assert(
      a.teleporter.teleport(GlobalPlayer.OnOtherServer("Other", UUID.randomUUID()), destination(100), world.getUID, "b").isLeft
    )
  }
