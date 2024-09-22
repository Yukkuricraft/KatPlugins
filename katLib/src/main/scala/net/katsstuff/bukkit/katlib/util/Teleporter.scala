package net.katsstuff.bukkit.katlib.util

import java.util.UUID

import net.katsstuff.bukkit.katlib.GlobalPlayer
import net.katsstuff.bukkit.katlib.text.*
import org.bukkit.event.player.PlayerTeleportEvent.TeleportCause
import org.bukkit.{Bukkit, Location}

trait Teleporter:
  def teleport(player: GlobalPlayer, destination: Location, worldUuid: UUID, server: String): Either[String, Unit]

object Teleporter:
  class SameServerTeleporter(currentServerName: String) extends Teleporter:
    def teleportReportingError(
        player: GlobalPlayer,
        destination: Location,
        worldUuid: UUID,
        server: String
    ): Either[String, Unit] =
      val res = teleport(player, destination, worldUuid, server)
      res.swap.foreach { e =>
        player match
          case GlobalPlayer.OnThisServer(player) => player.sendMessage(t"${TextColor.Red}$e")
          case _                                 =>
      }
      res

    override def teleport(
        player: GlobalPlayer,
        destination: Location,
        worldUuid: UUID,
        server: String
    ): Either[String, Unit] =
      player match
        case GlobalPlayer.OnThisServer(player) if server == currentServerName =>
          Option(Bukkit.getWorld(worldUuid)).toRight("Teleport failed. World not found").flatMap { world =>
            val newDestination = new Location(
              world,
              destination.getX,
              destination.getY,
              destination.getZ,
              destination.getYaw,
              destination.getPitch
            )

            if !newDestination.getChunk.load() then Left("Failed to load destination for teleport")
            else
              Either
                .cond(player.teleport(newDestination, TeleportCause.COMMAND), (), "Teleport failed for unknown reason")
          }
        case _ => Left("Cross server teleportation is not enabled")
