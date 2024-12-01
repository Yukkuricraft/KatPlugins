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
import io.papermc.paper.plugin.lifecycle.event.types.LifecycleEvents
import natchez.Trace
import natchez.Trace.Implicits.noop
import net.katsstuff.bukkit.katlib.command.HelpCmd
import net.katsstuff.bukkit.katlib.db.{CrossServerPostgresTeleporter, DbUpdates, ScalaDbPlugin}
import net.katsstuff.bukkit.katlib.util.Teleporter
import net.katsstuff.bukkit.katlib.{BungeeChannel, ScalaPlugin}
import net.katsstuff.bukkit.magicalwarps.WarpsConfig.{CrossServerCommunication, StorageType}
import net.katsstuff.bukkit.magicalwarps.cmd.*
import net.katsstuff.bukkit.magicalwarps.warp.storage.{SingleFileWarpStorage, WarpStorage}
import org.bukkit.Bukkit
import skunk.Session

class WarpsPlugin extends ScalaPlugin, ScalaDbPlugin {

  override val commandMapDefaultFallbackPrefix: String = "magicalwarps"

  given plugin: WarpsPlugin = this

  private var warpsConfig: WarpsConfig = uninitialized
  private var storage: WarpStorage     = uninitialized

  given WarpsConfig = warpsConfig
  given WarpStorage = storage

  def exportImportPath: Path = dataFolder.toPath.resolve("export.json")

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

    dbObjs =
      if !warpsConfig.storage.postgres.use then None
      else
        Some {
          val dbConfig      = warpsConfig.storage.postgres
          given Network[IO] = Network.forIO

          dispatcher.unsafeRunSync(
            DbUpdates.updateIfNeeded(presentDbVersion = 1)(
              using
              SkunkSessionPoolDb[IO](
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

  // noinspection UnstableApiUsage
  def setup(ignoreOneTime: Boolean): Unit =
    if ignoreOneTime then runKatLibRepeatableSetup()
    else runKatLibSetup()

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
            CrossServerPostgresTeleporter(pool, warpsConfig.serverName)
          case None => throw new Exception("Misssing database configuration for Postgres cross server communication")
        }

      case CrossServerCommunication.Single =>
        Teleporter.SameServerTeleporter(warpsConfig.serverName)
    }
    given Teleporter = teleporterVal

    if !ignoreOneTime then
      val helpCmd     = new HelpCmd(this)
      val warpCommand = Commands.warp(helpCmd.helpExecution)
      val warpsCommand = Commands.warps(
        helpExecution = helpCmd.helpExecution,
        reloadData = () => {
          onDisable()
          setup(ignoreOneTime = true)
        }
      )

      helpCmd.registerCommand(warpCommand)
      helpCmd.registerCommand(warpsCommand)

      getLifecycleManager.registerEventHandler(
        LifecycleEvents.COMMANDS.newHandler: event =>
          warpCommand.registerBrigadier(event.registrar, this)
          warpsCommand.registerBrigadier(event.registrar, this)
      )
  end setup

  override def onEnable(): Unit =
    setup(ignoreOneTime = false)

  override def onDisable(): Unit = runDisableActions()
}
