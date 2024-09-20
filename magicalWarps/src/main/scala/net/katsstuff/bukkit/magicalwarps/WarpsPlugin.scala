package net.katsstuff.bukkit.magicalwarps

import java.nio.file.Path
import java.util.logging.Logger
import javax.sql.DataSource
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}
import cats.effect.IO
import cats.effect.std.Dispatcher
import cats.effect.unsafe.IORuntime
import cats.syntax.all.*
import dataprism.platform.implementations.PostgresQueryPlatform
import dataprism.skunk.sql.SkunkDb
import dataprism.sql.Db
import fs2.io.net.Network
import natchez.Trace.Implicits.noop
import net.katsstuff.bukkit.katlib.ScalaPlugin
import net.katsstuff.bukkit.katlib.command.HelpCmd
import net.katsstuff.bukkit.magicalwarps.WarpsConfig.{CrossServerCommunication, StorageType}
import net.katsstuff.bukkit.magicalwarps.cmd.*
import net.katsstuff.bukkit.magicalwarps.warp.storage.{SingleFileWarpStorage, WarpStorage}
import org.bukkit.Bukkit
import skunk.Session

class WarpsPlugin extends ScalaPlugin {

  given plugin: WarpsPlugin = this

  private var warpsConfig: WarpsConfig = _
  private var storage: WarpStorage     = _

  given WarpsConfig = warpsConfig
  given WarpStorage = storage

  def exportImportPath: Path = dataFolder.toPath.resolve("export.json")

  private var _dispatcher: Dispatcher[IO] = _
  def dispatcher: Dispatcher[IO]          = _dispatcher

  private var dbObjs: Option[(Session[IO], Db[Future, skunk.Codec])] = _

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

          def makeSession() = {
            val (session, closeSession) = dispatcher.unsafeRunSync(
              skunk.Session
                .single[IO](
                  host = dbConfig.host,
                  port = dbConfig.port,
                  user = dbConfig.user,
                  database = dbConfig.database,
                  password = dbConfig.password,
                  parameters = dbConfig.parameters
                )
                .allocated
            )

            addDisableAction(closeSession.unsafeRunSync()(IORuntime.global))
            session
          }

          val otherSession = makeSession()
          DbUpdates.updateIfNeeded()(using SkunkDb[IO](otherSession), otherSession, this)

          val dbSession = makeSession()

          val db: Db[Future, skunk.Codec] =
            SkunkDb[IO](dbSession).mapK([X] => (fx: IO[X]) => dispatcher.unsafeToFuture(fx))

          (otherSession, db)
        }

    warpsConfig.storage.`type` match
      case StorageType.SingleFile => new SingleFileWarpStorage(dataFolder.toPath.resolve("storage.json"))

      case StorageType.Postgres =>
        dbObjs match {
          case Some((given Session[IO], given Db[Future, skunk.Codec])) =>
            ??? // new PostgresWarpStorage(new PostgresQueryPlatform)

          case None => throw new Exception("Misssing database configuration for Postgres storage")
        }

  override def onEnable(): Unit = {
    runKatLibSetup()

    warpsConfig = loadConfig().get

    storage = makeStorage()
    storage.reloadWarps().failed.foreach(logger.error("Failed to reload warps", _))

    given bungeeChannel: BungeeChannel = new BungeeChannel()

    this.getServer.getMessenger.registerOutgoingPluginChannel(this, "bungeecord:hsh")
    this.getServer.getMessenger.registerIncomingPluginChannel(this, "bungeecord:hsh", bungeeChannel)

    addDisableAction {
      this.getServer.getMessenger.unregisterOutgoingPluginChannel(this)
      this.getServer.getMessenger.unregisterIncomingPluginChannel(this)
    }

    given Teleporter = warpsConfig.crossServerCommunication match {
      case CrossServerCommunication.Postgres =>
        dbObjs match {
          case Some((given Session[IO], given Db[Future, skunk.Codec])) =>
            Teleporter.crossServerPostgresTeleporter
          case None => throw new Exception("Misssing database configuration for Postgres cross server communication")
        }

      case CrossServerCommunication.Single => Teleporter.sameServerTeleporter
    }

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
