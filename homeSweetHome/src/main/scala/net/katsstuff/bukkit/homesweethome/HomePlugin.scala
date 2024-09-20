package net.katsstuff.bukkit.homesweethome

import java.nio.file.{Files, Path}
import scala.compiletime.uninitialized
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}
import cats.effect.IO
import cats.effect.kernel.Resource
import cats.effect.std.Dispatcher
import cats.effect.unsafe.IORuntime
import dataprism.skunk.sql.{SkunkSessionDb, SkunkSessionPoolDb}
import dataprism.sql.Db
import fs2.io.net.Network
import io.papermc.paper.plugin.lifecycle.event.types.LifecycleEvents
import natchez.Trace
import natchez.Trace.Implicits.noop
import net.katsstuff.bukkit.homesweethome.HSHConfig.{CrossServerCommunication, StorageType}
import net.katsstuff.bukkit.homesweethome.cmd.*
import net.katsstuff.bukkit.homesweethome.home.homehandler.{HomeHandler, PostgresHomeHandler, SingleServerHomeHandler}
import net.katsstuff.bukkit.homesweethome.home.storage.{HomeStorage, MultiFileHomeStorage, PostgresHomeStorage, SingleFileHomeStorage}
import net.katsstuff.bukkit.katlib.ScalaPlugin
import net.katsstuff.bukkit.katlib.command.{CommandRegistrationType, HelpCmd}
import org.bukkit.Bukkit
import skunk.Session

class HomePlugin extends ScalaPlugin:

  override val commandMapDefaultFallbackPrefix: String = "homesweethome"

  given plugin: HomePlugin = this

  private var hshConfig: HSHConfig = uninitialized
  private var storage: HomeStorage = uninitialized

  given HSHConfig = hshConfig

  def exportImportPath: Path = dataFolder.toPath.resolve("export.json")

  private var _dispatcher: Dispatcher[IO] = uninitialized
  def dispatcher: Dispatcher[IO]          = _dispatcher

  private var dbObjs: Option[(Resource[IO, Session[IO]], Db[Future, skunk.Codec])] = uninitialized

  def loadConfig(): Try[HSHConfig] =
    HSHConfig
      .load()
      .toTry match
      case Success(t) => Success(t)
      case Failure(e) =>
        logger.error("Couldn't load config", e)
        Failure(e)

  def makeStorage(): HomeStorage =
    val (dispatcher, closeDispatcher) = Dispatcher.parallel[IO].allocated.unsafeRunSync()(using IORuntime.global)
    this._dispatcher = dispatcher

    addDisableAction(closeDispatcher.unsafeRunSync()(IORuntime.global))

    dbObjs =
      if !hshConfig.storage.postgres.use then None
      else
        Some {
          val dbConfig      = hshConfig.storage.postgres
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

    hshConfig.storage.`type` match
      case StorageType.SingleFile => new SingleFileHomeStorage(dataFolder.toPath.resolve("storage.json"))
      case StorageType.MultiFile =>
        val storagePath = dataFolder.toPath.resolve("storage")
        Files.createDirectories(storagePath)
        val storage = new MultiFileHomeStorage(storagePath)
        Bukkit.getPluginManager.registerEvents(storage, this)
        storage

      case StorageType.Postgres =>
        val storage = dbObjs match {
          case Some((_, given Db[Future, skunk.Codec])) =>
            new PostgresHomeStorage

          case None => throw new Exception("Misssing database configuration for Postgres storage")
        }

        Bukkit.getPluginManager.registerEvents(storage, this)
        storage
        
  //noinspection UnstableApiUsage
  def setup(ignoreOneTime: Boolean): Unit =
    if ignoreOneTime then
      runKatLibRepeatableSetup()
    else 
      runKatLibSetup()

    hshConfig = loadConfig().get

    storage = makeStorage()
    storage.reloadHomeData().failed.foreach(logger.error("Failed to reload home data", _))

    val bungeeChannelVal               = new BungeeChannel()
    given bungeeChannel: BungeeChannel = bungeeChannelVal

    this.getServer.getMessenger.registerOutgoingPluginChannel(this, "bungeecord:homesweethome")
    this.getServer.getMessenger.registerIncomingPluginChannel(this, "bungeecord:homesweethome", bungeeChannel)

    addDisableAction {
      this.getServer.getMessenger.unregisterOutgoingPluginChannel(this)
      this.getServer.getMessenger.unregisterIncomingPluginChannel(this)
    }

    val homeHandlerVal = hshConfig.crossServerCommunication match
      case CrossServerCommunication.Postgres =>
        dbObjs match
          case Some((pool, given Db[Future, skunk.Codec])) =>
            val handler = new PostgresHomeHandler(storage, pool)
            Bukkit.getPluginManager.registerEvents(handler, this)

            addDisableAction {
              handler.close()
            }

            handler
          case None => throw new Exception("Misssing database configuration for Postgres cross server communication")

      case CrossServerCommunication.Single =>
        new SingleServerHomeHandler(storage, hshConfig)
    given HomeHandler = homeHandlerVal

    val teleporterVal = hshConfig.crossServerCommunication match {
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
    
    if !ignoreOneTime then
      val homeHelp = new HelpCmd(this)
      val homeCmd = HomeCommands.homeCommand(
        helpExecution = homeHelp.helpExecution,
        reloadData = () => {
          onDisable()
          setup(ignoreOneTime = true)
        }
      )
      val homesCmd = HomeCommands.homesCommand
  
      homeHelp.registerCommand(homeCmd)
      homeHelp.registerCommand(homesCmd)
  
      getLifecycleManager.registerEventHandler(
        LifecycleEvents.COMMANDS.newHandler: event =>
          homeCmd.registerBrigadier(event.registrar, this)
          homesCmd.registerBrigadier(event.registrar, this)
      )
  end setup
  
  override def onEnable(): Unit =
    setup(ignoreOneTime = false)

  override def onDisable(): Unit = runDisableActions()
