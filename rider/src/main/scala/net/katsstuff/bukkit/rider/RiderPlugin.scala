package net.katsstuff.bukkit.rider

import io.papermc.paper.plugin.lifecycle.event.types.LifecycleEvents
import net.katsstuff.bukkit.katlib.ScalaPlugin
import net.katsstuff.bukkit.katlib.command.{Command, Senders}
import net.katsstuff.bukkit.katlib.text.*
import org.bukkit.Material
import org.bukkit.event.player.PlayerInteractEntityEvent
import org.bukkit.event.{EventHandler, Listener}

//noinspection UnstableApiUsage
class RiderPlugin extends ScalaPlugin with Listener:

  implicit val plugin: RiderPlugin = this

  private def ejectCommand = Command("eject", "rider.ride")(
    Command.execution(sender = Senders.player) { (player, _) =>
      if player.eject()
      then
        player.sendMessage(t"${Green}Ejected")
        Right(())
      else Left("No entities currently riding you")
    }
  )

  override def onEnable(): Unit =
    server.getPluginManager.registerEvents(this, this)
    getLifecycleManager.registerEventHandler(
      LifecycleEvents.COMMANDS.newHandler(event => ejectCommand.registerBrigadier(event.registrar, this))
    )

  override def onDisable(): Unit =
    PlayerInteractEntityEvent.getHandlerList.unregister(this: Listener)

  @EventHandler(ignoreCancelled = true)
  def onInteract(event: PlayerInteractEntityEvent): Unit =
    val player = event.getPlayer
    val entity = event.getRightClicked

    val heldItemstack = player.getInventory.getItem(event.getHand)
    if heldItemstack.getType == Material.SADDLE
      && player.hasPermission("rider.ride")
    then
      val (walker, passenger) =
        if player.isSneaking
        then (player, entity)
        else (entity, player)

      walker.addPassenger(passenger)
