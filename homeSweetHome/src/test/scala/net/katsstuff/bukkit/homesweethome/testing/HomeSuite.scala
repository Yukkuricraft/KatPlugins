package net.katsstuff.bukkit.homesweethome.testing

import java.time.temporal.ChronoUnit
import java.util.UUID

import scala.concurrent.duration.*
import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import dataprism.skunk.sql.SkunkSessionPoolDb
import dataprism.sql.Db
import io.circe.yaml.parser
import net.katsstuff.bukkit.homesweethome.home.Home
import net.katsstuff.bukkit.homesweethome.home.storage.PostgresHomeStorage
import net.katsstuff.bukkit.homesweethome.{HSHConfig, HomePlugin}
import net.katsstuff.bukkit.katlib.db.DbUpdates
import net.katsstuff.bukkit.katlib.db.testing.TestPostgres.TestDatabase
import net.katsstuff.bukkit.katlib.db.testing.{DbSuite, TestPostgres}
import net.katsstuff.bukkit.katlib.util.FutureOrNow
import org.bukkit.Bukkit
import skunk.Codec

trait HomeSuite extends DbSuite:

  val presentDbVersion = 2

  /** The default config, as if running on the given server. */
  def hshConfig(serverName: String = "a"): HSHConfig =
    val content = Source.fromResource("config.yml").mkString
    parser.parse(content).flatMap(_.as[HSHConfig]).toTry.get.copy(serverName = serverName)

  def createHomePlugin(name: String = "HomeSweetHome"): HomePlugin =
    createPlugin[HomePlugin](name, classOf[TestHomePlugin])

  /** Applies HomeSweetHome's own database updates, the same way the plugin does. */
  def migrate(database: TestDatabase, version: Int = presentDbVersion): Unit =
    given HomePlugin = createHomePlugin("HomeSweetHomeMigrations")
    DbUpdates.updateIfNeeded(version, database.schema)(using SkunkSessionPoolDb[IO](database.single)).unsafeRunSync()

  def migratedDatabase(): TestDatabase =
    // The plugin creates the schema itself
    val database = TestPostgres.freshDatabase(schema = "homesweethome", createSchema = false)
    migrate(database)
    database

  case class HomeServer(plugin: HomePlugin, config: HSHConfig, db: Db[Future, Codec], storage: PostgresHomeStorage):
    given HSHConfig = config

  /** A Postgres home storage as on the given server, registered as a listener. */
  def startHomeServer(database: TestDatabase, serverName: String = "a"): HomeServer =
    given plugin: HomePlugin = createHomePlugin(s"HomeSweetHome-$serverName")
    given config: HSHConfig  = hshConfig(serverName)
    val connection           = connect(database, plugin)
    given Db[Future, Codec]  = connection.db
    val storage              = new PostgresHomeStorage
    Bukkit.getPluginManager.registerEvents(storage, plugin)
    closeAfterTest(storage.close())
    HomeServer(plugin, config, connection.db, storage)

  def result[A](f: FutureOrNow[A]): A = await(f.asFuture)

  /** The database only keeps microseconds. */
  def truncated(home: Home): Home =
    home.copy(
      createdAt = home.createdAt.truncatedTo(ChronoUnit.MICROS),
      updatedAt = home.updatedAt.truncatedTo(ChronoUnit.MICROS)
    )
