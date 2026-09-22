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
import net.katsstuff.bukkit.magicalwarps.warp.storage.{PostgresWarpStorage, SingleFileWarpStorage, WarpStorage}
import org.bukkit.Bukkit
import org.bukkit.event.HandlerList
import skunk.Session

class WarpsPlugin extends ScalaPlugin, ScalaDbPlugin {

  override val commandMapDefaultFallbackPrefix: String = "magicalwarps"

  given plugin: WarpsPlugin = this

  val context: WarpsContext = new WarpsContext
  given WarpsContext        = context

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
      if !context.config.storage.postgres.use then None
      else
        Some {
          val dbConfig      = context.config.storage.postgres
          given Network[IO] = Network.forIO

          dispatcher.unsafeRunSync(
            DbUpdates.updateIfNeeded(presentDbVersion = 1, dbConfig.schema)(
              using SkunkSessionPoolDb[IO](
                skunk.Session.single[IO](
                  host = dbConfig.host,
                  port = dbConfig.port,
                  user = dbConfig.user,
                  database = dbConfig.database,
                  password = dbConfig.password,
                  parameters = dbConfig.connectionParameters
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
                  parameters = dbConfig.connectionParameters
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

    context.config.storage.`type` match
      case StorageType.SingleFile =>
        new SingleFileWarpStorage(dataFolder.toPath.resolve("storage.json"))(using this, context.config)

      case StorageType.Postgres =>
        dbObjs match {
          case Some((pool, given Db[Future, skunk.Codec])) =>
            val storage = new PostgresWarpStorage(pool)
            addDisableAction(storage.close())
            storage

          case None => throw new Exception("Misssing database configuration for Postgres storage")
        }

  // noinspection UnstableApiUsage
  def setup(ignoreOneTime: Boolean): Unit =
    if ignoreOneTime then runKatLibRepeatableSetup()
    else runKatLibSetup()

    context.config = loadConfig().get

    context.storage = makeStorage()
    context.storage.reloadWarps().failed.foreach(logger.error("Failed to reload warps", _))

    val bungeeChannelVal               = new BungeeChannel()
    given bungeeChannel: BungeeChannel = bungeeChannelVal

    // BungeeChannel talks to the proxy on its channel, which plugins must register to be allowed to send on
    this.getServer.getMessenger.registerOutgoingPluginChannel(this, "BungeeCord")
    this.getServer.getMessenger.registerIncomingPluginChannel(this, "BungeeCord", bungeeChannel)

    addDisableAction {
      this.getServer.getMessenger.unregisterOutgoingPluginChannel(this)
      this.getServer.getMessenger.unregisterIncomingPluginChannel(this)
    }

    context.teleporter = context.config.crossServerCommunication match {
      case CrossServerCommunication.Postgres =>
        dbObjs match {
          case Some((pool, given Db[Future, skunk.Codec])) =>
            val postgresTeleporter =
              CrossServerPostgresTeleporter(pool, context.config.serverName, "magicalwarps_delayed_teleport_change")
            Bukkit.getPluginManager.registerEvents(postgresTeleporter, this)

            addDisableAction {
              HandlerList.unregisterAll(postgresTeleporter)
              postgresTeleporter.close()
            }

            postgresTeleporter
          case None => throw new Exception("Misssing database configuration for Postgres cross server communication")
        }

      case CrossServerCommunication.Single =>
        Teleporter.SameServerTeleporter(context.config.serverName)
    }

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
