package net.katsstuff.bukkit.homesweethome.home.homehandler

import java.util.UUID

import scala.concurrent.Future

import dataprism.sql.Db
import io.circe.syntax.*
import net.katsstuff.bukkit.homesweethome.home.Home
import net.katsstuff.bukkit.homesweethome.testing.HomeSuite
import net.katsstuff.bukkit.homesweethome.{HSHConfig, HomePlugin}
import net.katsstuff.bukkit.katlib.GlobalPlayer
import net.katsstuff.bukkit.katlib.db.testing.TestPostgres.TestDatabase
import org.bukkit.{Bukkit, Location}
import skunk.codec.all.*
import skunk.implicits.*
import skunk.Codec

class PostgresHomeHandlerTest extends HomeSuite:

  case class HandlerServer(name: String, handler: PostgresHomeHandler)

  private def startHandler(database: TestDatabase, serverName: String): HandlerServer =
    val homeServer           = startHomeServer(database, serverName)
    given HomePlugin         = homeServer.plugin
    given HSHConfig          = homeServer.config
    given Db[Future, Codec]  = homeServer.db
    val connection           = connect(database, homeServer.plugin)
    val handler              = new PostgresHomeHandler(homeServer.storage, connection.pool)
    Bukkit.getPluginManager.registerEvents(handler, homeServer.plugin)
    closeAfterTest(handler.close())
    // Wait for the initial loads
    handler.globalOnlinePlayers
    handler.getRequest(UUID.randomUUID(), UUID.randomUUID())
    handler.isInvited(UUID.randomUUID(), UUID.randomUUID(), makeHome(UUID.randomUUID(), "x"))
    HandlerServer(serverName, handler)

  private def makeHome(owner: UUID, name: String, x: Double = 10): Home =
    Home.makeNew(owner, name, new Location(server.getWorlds.get(0), x, 64, 0))(using hshConfig())

  override def beforeEach(context: BeforeEach): Unit =
    super.beforeEach(context)
    server.addSimpleWorld("world")

  private def insertRequest(database: TestDatabase, table: String, target: UUID, owner: UUID, home: Home, expires: String): Unit =
    database.executeSql(
      s"INSERT INTO $table VALUES ('$target', '$owner', '${home.asJson.noSpaces}', $expires)"
    )

  private def count(database: TestDatabase, table: String): Long =
    database.query(sql"SELECT count(*) FROM #$table".query(int8)).head

  test("requests made on one server are seen on the others") {
    val database  = migratedDatabase()
    val a         = startHandler(database, "a")
    val b         = startHandler(database, "b")
    val requester = UUID.randomUUID()
    val owner     = UUID.randomUUID()
    val home      = makeHome(owner, "home")

    result(a.handler.addRequest(requester, owner, home))
    eventually()(b.handler.getRequest(requester, owner).contains(home))
    eventually()(a.handler.getRequest(requester, owner).contains(home))
    assertEquals(b.handler.getAllRequestersForPlayer(owner), Seq(requester))
  }

  test("removed requests are removed on the other servers") {
    val database  = migratedDatabase()
    val a         = startHandler(database, "a")
    val b         = startHandler(database, "b")
    val requester = UUID.randomUUID()
    val owner     = UUID.randomUUID()

    result(a.handler.addRequest(requester, owner, makeHome(owner, "home")))
    eventually()(b.handler.getRequest(requester, owner).isDefined)
    result(a.handler.removeRequest(requester, owner))
    eventually()(b.handler.getRequest(requester, owner).isEmpty)
    assertEquals(b.handler.getAllRequestersForPlayer(owner), Seq())
  }

  test("making a new request replaces the old one") {
    val database  = migratedDatabase()
    val a         = startHandler(database, "a")
    val b         = startHandler(database, "b")
    val requester = UUID.randomUUID()
    val owner     = UUID.randomUUID()
    val newHome   = makeHome(owner, "new")

    result(a.handler.addRequest(requester, owner, makeHome(owner, "old")))
    result(a.handler.addRequest(requester, owner, newHome))
    eventually()(b.handler.getRequest(requester, owner).contains(newHome))
    assertEquals(count(database, "requests"), 1L)
  }

  test("requesters are found for the right home owner") {
    val database = migratedDatabase()
    val a        = startHandler(database, "a")
    val owner    = UUID.randomUUID()
    val other    = UUID.randomUUID()
    val (r1, r2, r3) = (UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID())

    result(a.handler.addRequest(r1, owner, makeHome(owner, "home")))
    result(a.handler.addRequest(r2, owner, makeHome(owner, "home")))
    result(a.handler.addRequest(r3, other, makeHome(other, "home")))
    eventually()(a.handler.getAllRequestersForPlayer(owner).toSet == Set(r1, r2))
    assertEquals(a.handler.getAllRequestersForPlayer(other), Seq(r3))
  }

  test("requests that haven't expired are loaded when starting") {
    val database  = migratedDatabase()
    val requester = UUID.randomUUID()
    val owner     = UUID.randomUUID()
    val home      = makeHome(owner, "home")
    insertRequest(database, "requests", requester, owner, home, "now() + interval '1 minute'")

    val a = startHandler(database, "a")
    assertEquals(a.handler.getRequest(requester, owner), Some(home))
  }

  test("expired requests are removed, and not loaded, when starting") {
    val database  = migratedDatabase()
    val requester = UUID.randomUUID()
    val owner     = UUID.randomUUID()
    insertRequest(database, "requests", requester, owner, makeHome(owner, "home"), "now() - interval '1 minute'")

    val a = startHandler(database, "a")
    assertEquals(a.handler.getRequest(requester, owner), None)
    eventually()(count(database, "requests") == 0L)
  }

  test("invites made on one server are seen on the others") {
    val database = migratedDatabase()
    val a        = startHandler(database, "a")
    val b        = startHandler(database, "b")
    val target   = UUID.randomUUID()
    val owner    = UUID.randomUUID()
    val home     = makeHome(owner, "home")

    result(a.handler.addInvite(target, owner, home))
    eventually()(b.handler.isInvited(target, owner, home))
    assert(!b.handler.isInvited(target, owner, makeHome(owner, "other")))
    assert(!b.handler.isInvited(UUID.randomUUID(), owner, home))

    result(b.handler.removeInvite(target, owner))
    eventually()(!a.handler.isInvited(target, owner, home))
  }

  test("invites that haven't expired are loaded when starting, and expired ones removed") {
    val database = migratedDatabase()
    val owner    = UUID.randomUUID()
    val valid    = UUID.randomUUID()
    val expired  = UUID.randomUUID()
    val home     = makeHome(owner, "home")
    insertRequest(database, "invites", valid, owner, home, "now() + interval '1 minute'")
    insertRequest(database, "invites", expired, owner, home, "now() - interval '1 minute'")

    val a = startHandler(database, "a")
    assert(a.handler.isInvited(valid, owner, home))
    assert(!a.handler.isInvited(expired, owner, home))
    eventually()(count(database, "invites") == 1L)
  }

  test("an invite to a home read from the database matches the home in memory") {
    val database = migratedDatabase()
    val a        = startHandler(database, "a")
    val owner    = server.addPlayer()
    val target   = UUID.randomUUID()

    // The owner is online, so the home comes from memory
    result(a.handler.makeHome(owner.getUniqueId, "home", new Location(server.getWorlds.get(0), 1.23456789, 64, 0)))
    val home = result(a.handler.specificHome(owner.getUniqueId, "home")).get
    result(a.handler.addInvite(target, owner.getUniqueId, home))

    // Another server reads the home from the database
    val b      = startHandler(database, "b")
    val fromDb = result(b.handler.specificHome(owner.getUniqueId, "home")).get
    eventually()(b.handler.isInvited(target, owner.getUniqueId, fromDb))
  }

  test("players joining one server are seen as online on the others") {
    val database = migratedDatabase()
    startHandler(database, "a")
    val b      = startHandler(database, "b")
    val player = server.addPlayer()

    eventually()(b.handler.globalOnlinePlayers.exists(_.uuid == player.getUniqueId))
    eventually()(count(database, "online_players") == 1L)
  }

  test("players on other servers are seen as such") {
    val database = migratedDatabase()
    val a        = startHandler(database, "a")
    val uuid     = UUID.randomUUID()
    database.executeSql(s"INSERT INTO online_players VALUES ('$uuid', 'Elsewhere', 'c')")

    eventually()(a.handler.globalOnlinePlayers.contains(GlobalPlayer.OnOtherServer("Elsewhere", uuid)))
    database.executeSql(s"DELETE FROM online_players WHERE uuid = '$uuid'")
    eventually()(!a.handler.globalOnlinePlayers.exists(_.uuid == uuid))
  }

  test("players leaving are removed") {
    val database = migratedDatabase()
    val a        = startHandler(database, "a")
    val player   = server.addPlayer()
    eventually()(a.handler.globalOnlinePlayers.exists(_.uuid == player.getUniqueId))

    player.disconnect()
    eventually()(!a.handler.globalOnlinePlayers.exists(_.uuid == player.getUniqueId))
    eventually()(count(database, "online_players") == 0L)
  }

  test("reloading replaces this server's online players, and leaves the other servers alone") {
    val database = migratedDatabase()
    val a        = startHandler(database, "a")
    val stale    = UUID.randomUUID()
    val other    = UUID.randomUUID()
    database.executeSql(s"INSERT INTO online_players VALUES ('$stale', 'Stale', 'a'), ('$other', 'Other', 'c')")
    val player = new be.seeseemelk.mockbukkit.entity.PlayerMock(server, "Online", UUID.randomUUID())
    // Added without the handler seeing the join
    org.bukkit.event.HandlerList.unregisterAll(a.handler)
    server.addPlayer(player)

    await(a.handler.reload())
    val rows = database.query(sql"SELECT name, server FROM online_players ORDER BY name".query(text ~ text))
    assertEquals(rows, List("Online" -> "a", "Other" -> "c"))
  }
