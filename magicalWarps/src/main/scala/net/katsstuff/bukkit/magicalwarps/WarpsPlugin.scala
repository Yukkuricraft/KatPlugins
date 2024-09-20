package net.katsstuff.bukkit.magicalwarps

import java.nio.file.Path
import java.util.logging.Logger
import javax.sql.DataSource

import scala.compiletime.uninitialized
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.effect.std.Dispatcher
import cats.effect.unsafe.IORuntime
import cats.syntax.all.*
import dataprism.skunk.sql.{SkunkSessionDb, SkunkSessionPoolDb}
import dataprism.sql.Db
import fs2.io.net.Network
import natchez.Trace
import natchez.Trace.Implicits.noop
import net.katsstuff.bukkit.katlib.ScalaPlugin
import net.katsstuff.bukkit.katlib.command.HelpCmd
import net.katsstuff.bukkit.magicalwarps.WarpsConfig.{CrossServerCommunication, StorageType}
import net.katsstuff.bukkit.magicalwarps.cmd.*
import net.katsstuff.bukkit.magicalwarps.warp.storage.{SingleFileWarpStorage, WarpStorage}
import org.bukkit.Bukkit
import skunk.Session

class WarpsPlugin extends ScalaPlugin {

  override val useCommandmap: Boolean                  = true
  override val commandMapDefaultFallbackPrefix: String = "magicalwarps"

  given plugin: WarpsPlugin = this

  private var warpsConfig: WarpsConfig = uninitialized
  private var storage: WarpStorage     = uninitialized

  given WarpsConfig = warpsConfig
  given WarpStorage = storage

  def exportImportPath: Path = dataFolder.toPath.resolve("export.json")

  private var _dispatcher: Dispatcher[IO] = uninitialized
  def dispatcher: Dispatcher[IO]          = _dispatcher

  private var dbObjs: Option[(Resource[IO, Session[IO]], Db[Future, skunk.Codec])] = uninitialized

  def loadConfig(): Try[WarpsConfig] =
    WarpsConfig
      .load()
      .toTry match
      case Success(t) => Success(t)
      case Failure(e) =>
        logger.error("Couldn't load config", e)
        Failure(e)

  def makeStorage(): WarpStorage =
    val (dispatcher, closeDispatcher) = Dispatcher.parallel[IO].allocated.unsafeRunSync()(using IORuntime.global)
    this._dispatcher = dispatcher

    addDisableAction(closeDispatcher.unsafeRunSync()(IORuntime.global))

    dbObjs =
      if !warpsConfig.storage.postgres.use then None
      else
        Some {
          val dbConfig      = warpsConfig.storage.postgres
          given Network[IO] = Network.forIO

          dispatcher.unsafeRunSync(
            DbUpdates.updateIfNeeded()(
              using SkunkSessionPoolDb[IO](
                skunk.Session.single[IO](
                  host = dbConfig.host,
                  port = dbConfig.port,
                  user = dbConfig.user,
                  database = dbConfig.database,
                  password = dbConfig.password,
                  parameters = dbConfig.parameters
                )
              ),
              this
            )
          )

          val sessions =
            val (sessionPool, closeSession) = dispatcher.unsafeRunSync(
              skunk.Session
                .pooled[IO](
                  host = dbConfig.host,
                  port = dbConfig.port,
                  user = dbConfig.user,
                  database = dbConfig.database,
                  password = dbConfig.password,
                  max = dbConfig.maxConnections,
                  parameters = dbConfig.parameters
                )
                .allocated
            )

            addDisableAction(closeSession.unsafeRunSync()(IORuntime.global))
            sessionPool
          end sessions

          val db: Db[Future, skunk.Codec] =
            SkunkSessionPoolDb[IO](sessions).mapK([X] => (fx: IO[X]) => dispatcher.unsafeToFuture(fx))

          (sessions, db)
        }

    warpsConfig.storage.`type` match
      case StorageType.SingleFile => new SingleFileWarpStorage(dataFolder.toPath.resolve("storage.json"))

      case StorageType.Postgres =>
        dbObjs match {
          case Some((_, given Db[Future, skunk.Codec])) =>
            ??? // new PostgresWarpStorage(new PostgresQueryPlatform)

          case None => throw new Exception("Misssing database configuration for Postgres storage")
        }

  override def onEnable(): Unit = {
    runKatLibSetup()

    warpsConfig = loadConfig().get

    storage = makeStorage()
    storage.reloadWarps().failed.foreach(logger.error("Failed to reload warps", _))

    val bungeeChannelVal               = new BungeeChannel()
    given bungeeChannel: BungeeChannel = bungeeChannelVal

    this.getServer.getMessenger.registerOutgoingPluginChannel(this, "bungeecord:magicalwarps")
    this.getServer.getMessenger.registerIncomingPluginChannel(this, "bungeecord:magicalwarps", bungeeChannel)

    addDisableAction {
      this.getServer.getMessenger.unregisterOutgoingPluginChannel(this)
      this.getServer.getMessenger.unregisterIncomingPluginChannel(this)
    }

    val teleporterVal = warpsConfig.crossServerCommunication match {
      case CrossServerCommunication.Postgres =>
        dbObjs match {
          case Some((pool, given Db[Future, skunk.Codec])) =>
            Teleporter.crossServerPostgresTeleporter(pool)
          case None => throw new Exception("Misssing database configuration for Postgres cross server communication")
        }

      case CrossServerCommunication.Single =>
        Teleporter.sameServerTeleporter
    }
    given Teleporter = teleporterVal

    val helpCmd     = new HelpCmd(this)
    val warpCommand = Commands.warp(helpCmd.helpExecution)
    val warpsCommand = Commands.warps(
      helpExecution = helpCmd.helpExecution,
      reloadData = () => {
        onDisable()
        onEnable()
      }
    )

    helpCmd.registerCommand(warpCommand)
    helpCmd.registerCommand(warpsCommand)

    warpCommand.register(this)
    warpsCommand.register(this)
  }

  override def onDisable(): Unit = runDisableActions()
}
