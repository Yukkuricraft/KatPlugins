package net.katsstuff.bukkit.magicalwarps.testing

import java.util.UUID

import scala.concurrent.Future
import scala.io.Source

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import dataprism.skunk.sql.SkunkSessionPoolDb
import dataprism.sql.Db
import io.circe.yaml.parser
import net.katsstuff.bukkit.katlib.db.DbUpdates
import net.katsstuff.bukkit.katlib.db.testing.TestPostgres.TestDatabase
import net.katsstuff.bukkit.katlib.db.testing.{DbSuite, TestPostgres}
import net.katsstuff.bukkit.katlib.text.*
import net.katsstuff.bukkit.katlib.util.FutureOrNow
import net.katsstuff.bukkit.magicalwarps.warp.Warp
import net.katsstuff.bukkit.magicalwarps.warp.storage.PostgresWarpStorage
import net.katsstuff.bukkit.magicalwarps.{WarpsConfig, WarpsPlugin}
import skunk.Codec

trait WarpsSuite extends DbSuite:

  val presentDbVersion = 1

  /** The default config, as if running on the given server. */
  def warpsConfig(serverName: String = "a"): WarpsConfig =
    val content = Source.fromResource("config.yml").mkString
    parser.parse(content).flatMap(_.as[WarpsConfig]).toTry.get.copy(serverName = serverName)

  def createWarpsPlugin(name: String = "MagicalWarps"): WarpsPlugin =
    createPlugin[WarpsPlugin](name, classOf[TestWarpsPlugin])

  /** Applies MagicalWarps' own database updates, the same way the plugin does. */
  def migrate(database: TestDatabase, version: Int = presentDbVersion): Unit =
    given WarpsPlugin = createWarpsPlugin("MagicalWarpsMigrations")
    DbUpdates.updateIfNeeded(version, database.schema)(using SkunkSessionPoolDb[IO](database.single)).unsafeRunSync()

  def migratedDatabase(): TestDatabase =
    // The plugin creates the schema itself
    val database = TestPostgres.freshDatabase(schema = "magicalwarps", createSchema = false)
    migrate(database)
    database

  /** A Postgres warp storage as on the given server. */
  def startWarpServer(database: TestDatabase, serverName: String = "a"): PostgresWarpStorage =
    given plugin: WarpsPlugin = createWarpsPlugin(s"MagicalWarps-$serverName")
    val connection            = connect(database, plugin)
    given Db[Future, Codec]   = connection.db
    val storage               = new PostgresWarpStorage(connection.pool)
    closeAfterTest(storage.close())
    await(storage.reloadWarps())
    storage

  def result[A](f: FutureOrNow[A]): A = await(f.asFuture)

  def warp(name: String, x: Double = 10, world: UUID = UUID.randomUUID(), server: String = "a"): Warp =
    Warp(name, x, 64, 20, 90, 10, world, server, None, Seq.empty, Seq.empty, Seq.empty, None)

  /** A warp using every field. */
  def fullWarp(name: String): Warp =
    warp(name).copy(
      displayName = Some(t"${TextColor.Aqua}Fancy $name"),
      groups = Seq("towns", "spawns"),
      allowedPermGroups = Seq("admin"),
      allowedUsers = Seq(UUID.randomUUID(), UUID.randomUUID()),
      lore = Some(t"A place called $name")
    )
