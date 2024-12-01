package net.katsstuff.bukkit.cmdcallback

import java.nio.file.Files

import scala.compiletime.uninitialized
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try}

import com.destroystokyo.paper.event.player.PlayerJumpEvent
import io.circe.Decoder
import io.circe.derivation.Configuration
import io.circe.yaml.parser
import io.papermc.paper.event.player.{PlayerBedFailEnterEvent, PlayerDeepSleepEvent, PlayerStopUsingItemEvent}
import io.papermc.paper.plugin.lifecycle.event.types.LifecycleEvents
import net.katsstuff.bukkit.katlib.ScalaPlugin
import net.katsstuff.bukkit.katlib.command.*
import net.katsstuff.bukkit.katlib.text.*
import org.bukkit.Bukkit
import org.bukkit.command.CommandSender
import org.bukkit.entity.Player
import org.bukkit.event.player.{
  PlayerBedEnterEvent,
  PlayerBedLeaveEvent,
  PlayerExpChangeEvent,
  PlayerGameModeChangeEvent,
  PlayerJoinEvent,
  PlayerLevelChangeEvent,
  PlayerLoginEvent,
  PlayerQuitEvent,
  PlayerRespawnEvent,
  PlayerToggleSneakEvent,
  PlayerToggleSprintEvent
}
import org.bukkit.event.{EventHandler, Listener}

//noinspection UnstableApiUsage
object CmdCallbackPlugin extends ScalaPlugin with Listener:

  private var callbackConfig: Map[CmdCallbackTrigger, Seq[CmdCallback]] = uninitialized

  private def feelingsRelayCommand = Command("cmdcallback", "cmdcallback.admin")(
    Command.subCommand("reload")(
      Command.execution() { (sender: CommandSender, _) =>
        loadConfig().toTry match {
          case Success(newConfig) =>
            callbackConfig = newConfig
            sender.sendMessage(t"${Green}Config load success")
            Right(())

          case Failure(e) =>
            e.printStackTrace()
            Left(e.getMessage)
        }

      }
    ),
    Command.execution() { (sender: CommandSender, _) =>
      sender.sendMessage(t"${Yellow}CmdCallback v1.0")
      Right(())
    }
  )

  override def onEnable(): Unit =
    callbackConfig = loadConfig().toTry.get
    server.getPluginManager.registerEvents(this, this)
    getLifecycleManager.registerEventHandler(
      LifecycleEvents.COMMANDS.newHandler(e => feelingsRelayCommand.registerBrigadier(e.registrar, this))
    )

  override def onDisable(): Unit =
    Seq(
      PlayerStopUsingItemEvent.getHandlerList,
      PlayerBedLeaveEvent.getHandlerList,
      PlayerToggleSneakEvent.getHandlerList,
      PlayerJumpEvent.getHandlerList,
      PlayerQuitEvent.getHandlerList,
      PlayerRespawnEvent.getHandlerList,
      PlayerBedFailEnterEvent.getHandlerList,
      PlayerToggleSprintEvent.getHandlerList,
      PlayerLevelChangeEvent.getHandlerList,
      PlayerBedEnterEvent.getHandlerList,
      PlayerJoinEvent.getHandlerList,
      PlayerExpChangeEvent.getHandlerList,
      PlayerGameModeChangeEvent.getHandlerList,
      PlayerDeepSleepEvent.getHandlerList
    ).foreach(_.unregister(this: Listener))
    callbackConfig = null

  def loadConfig(): Either[Throwable, Map[CmdCallbackTrigger, Seq[CmdCallback]]] =
    val path = dataFolder.toPath.resolve("config.yml")
    logger.info("Loading Config")
    Try {
      Files.createDirectories(path.getParent)
      if Files.notExists(path) then saveResource("config.yml", false)
      Files.readAllLines(path).asScala.mkString("\n")
    }.toEither
      .flatMap(parser.parse)
      .flatMap { json =>
        val cursor = json.hcursor
        cursor.get[Int]("version").flatMap {
          case 1 => cursor.downField("callbacks").get[Seq[CmdCallback]]("callbacks").map(_.groupBy(_.trigger))
          case _ => Left(new Exception("Unsupported version"))
        }
      }

  private def runCallback(player: Player)(callback: CmdCallback): Unit =
    if callback.conditions.test(player) then Bukkit.dispatchCommand(player, callback.cmd)

  private def triggerCallbacks(player: Player, trigger: CmdCallbackTrigger): Unit =
    val (delayedCallbacks, nowCallbacks) = callbackConfig.getOrElse(trigger, Nil).view.partition(_.delay.isDefined)
    nowCallbacks.foreach(runCallback(player))
    delayedCallbacks.foreach { callback =>
      player.getScheduler.execute(this, () => runCallback(player)(callback), null, callback.delay.get)
    }

  @EventHandler(ignoreCancelled = true)
  def onStopUsingItem(ev: PlayerStopUsingItemEvent): Unit =
    triggerCallbacks(ev.getPlayer, CmdCallbackTrigger.PlayerStopUsingItemEvent)

  @EventHandler(ignoreCancelled = true)
  def onBedLeave(ev: PlayerBedLeaveEvent): Unit =
    triggerCallbacks(ev.getPlayer, CmdCallbackTrigger.PlayerBedLeaveEvent)

  @EventHandler(ignoreCancelled = true)
  def onToggleSneak(ev: PlayerToggleSneakEvent): Unit =
    triggerCallbacks(ev.getPlayer, CmdCallbackTrigger.PlayerToggleSneakEvent)

  @EventHandler(ignoreCancelled = true)
  def onJump(ev: PlayerJumpEvent): Unit =
    triggerCallbacks(ev.getPlayer, CmdCallbackTrigger.PlayerJumpEvent)

  @EventHandler(ignoreCancelled = true)
  def onQuit(ev: PlayerQuitEvent): Unit =
    triggerCallbacks(ev.getPlayer, CmdCallbackTrigger.PlayerQuitEvent)

  @EventHandler(ignoreCancelled = true)
  def onRespawn(ev: PlayerRespawnEvent): Unit =
    triggerCallbacks(ev.getPlayer, CmdCallbackTrigger.PlayerRespawnEvent)

  @EventHandler(ignoreCancelled = true)
  def onBedFailEnter(ev: PlayerBedFailEnterEvent): Unit =
    triggerCallbacks(ev.getPlayer, CmdCallbackTrigger.PlayerBedFailEnterEvent)

  @EventHandler(ignoreCancelled = true)
  def onToggleSprint(ev: PlayerToggleSprintEvent): Unit =
    triggerCallbacks(ev.getPlayer, CmdCallbackTrigger.PlayerToggleSprintEvent)

  @EventHandler(ignoreCancelled = true)
  def onLevelChange(ev: PlayerLevelChangeEvent): Unit =
    triggerCallbacks(ev.getPlayer, CmdCallbackTrigger.PlayerLevelChangeEvent)

  @EventHandler(ignoreCancelled = true)
  def onBedEnter(ev: PlayerBedEnterEvent): Unit =
    triggerCallbacks(ev.getPlayer, CmdCallbackTrigger.PlayerBedEnterEvent)

  @EventHandler(ignoreCancelled = true)
  def onJoin(ev: PlayerJoinEvent): Unit =
    triggerCallbacks(ev.getPlayer, CmdCallbackTrigger.PlayerJoinEvent)

  @EventHandler(ignoreCancelled = true)
  def onExpChange(ev: PlayerExpChangeEvent): Unit =
    triggerCallbacks(ev.getPlayer, CmdCallbackTrigger.PlayerExpChangeEvent)

  @EventHandler(ignoreCancelled = true)
  def onGameModeChange(ev: PlayerGameModeChangeEvent): Unit =
    triggerCallbacks(ev.getPlayer, CmdCallbackTrigger.PlayerGameModeChangeEvent)

  @EventHandler(ignoreCancelled = true)
  def onDeepSleep(ev: PlayerDeepSleepEvent): Unit =
    triggerCallbacks(ev.getPlayer, CmdCallbackTrigger.PlayerDeepSleepEvent)
