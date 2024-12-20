package net.katsstuff.bukkit.katlib

import java.io.{File, InputStream, Reader}
import java.util.Locale
import java.util.concurrent.Callable

import scala.collection.mutable
import scala.concurrent.ExecutionContext

import io.papermc.paper.plugin.lifecycle.event.types.LifecycleEvents
import net.katsstuff.bukkit.katlib.command.CommandRegistrationType.Brigadier
import net.katsstuff.bukkit.katlib.command.{CommandRegistrationType, PageCmd}
import net.katsstuff.bukkit.katlib.service.{PaginationService, SimplePagination}
import org.bukkit.configuration.file.FileConfiguration
import org.bukkit.plugin.ServicePriority
import org.bukkit.plugin.java.JavaPlugin
import org.bukkit.{Bukkit, Server}
import org.slf4j

/** A small convenience file to make stuff more scala like. */
class ScalaPlugin extends JavaPlugin { plugin =>

  val logger: slf4j.Logger = getSLF4JLogger

  val doWhenDisabling: mutable.Queue[() => Unit] = new mutable.Queue[() => Unit]

  val commandRegistrationType: CommandRegistrationType = CommandRegistrationType.Brigadier
  val commandMapDefaultFallbackPrefix: String          = getName.toLowerCase(Locale.ROOT)

  given ScalaPlugin = this

  val pageCmd = new PageCmd

  private val paginationServiceImpl = SimplePagination()

  given ExecutionContext = ExecutionContext.global

  val serverThreadExecutionContext: ExecutionContext = new ExecutionContext:
    override def execute(runnable: Runnable): Unit =
      Bukkit.getScheduler.callSyncMethod(plugin, () => runnable.run())

    override def reportFailure(cause: Throwable): Unit =
      logger.error(cause.getMessage, cause)

  protected def runKatLibRepeatableSetup(): Unit =
    Bukkit.getServicesManager.register(classOf[PaginationService], paginationServiceImpl, this, ServicePriority.Normal)
    addDisableAction {
      Bukkit.getServicesManager.unregister(classOf[PaginationService], paginationServiceImpl)
    }

  // noinspection UnstableApiUsage
  protected def runKatLibSetup(): Unit =
    runKatLibRepeatableSetup()

    commandRegistrationType match
      case CommandRegistrationType.Bukkit =>
        pageCmd.command.register(this)
      case CommandRegistrationType.CommandMap =>
        pageCmd.command.registerCommandMap(this, commandMapDefaultFallbackPrefix)
      case CommandRegistrationType.Brigadier =>
        getLifecycleManager.registerEventHandler(
          LifecycleEvents.COMMANDS.newHandler(event => pageCmd.command.registerBrigadier(event.registrar, this))
        )

  def runDisableActions(): Unit =
    while doWhenDisabling.nonEmpty do
      val action = doWhenDisabling.dequeue()
      action()
  end runDisableActions

  def addDisableAction(action: => Unit): Unit = doWhenDisabling.enqueue(() => action)

  override def onDisable(): Unit = runDisableActions()

  def dataFolder: File                                  = getDataFolder
  def server: Server                                    = getServer
  protected def file: File                              = getFile
  def config: FileConfiguration                         = getConfig
  def textResource(file: String): Reader                = getTextResource(file)
  def resource(filename: String): InputStream           = getResource(filename)
  final protected def classLoader: ClassLoader          = getClassLoader
  final protected def enabled_=(enabled: Boolean): Unit = setEnabled(enabled)
  final def naggable: Boolean                           = isNaggable
  final def naggable_=(canNag: Boolean): Unit           = setNaggable(canNag)
}
