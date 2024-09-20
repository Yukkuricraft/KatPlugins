package net.katsstuff.bukkit.homesweethome.home

import java.time.Instant
import java.util.UUID

import io.circe.*
import net.katsstuff.bukkit.homesweethome.HSHConfig
import org.bukkit.entity.Player
import org.bukkit.event.player.PlayerTeleportEvent.TeleportCause
import org.bukkit.{Bukkit, Location, World}

case class OldHome(x: Double, y: Double, z: Double, yaw: Float, pitch: Float, worldUuid: UUID, residents: Set[UUID])
    derives Codec.AsObject:

  def this(x: Double, y: Double, z: Double, yaw: Float, pitch: Float, world: World) =
    this(x, y, z, yaw, pitch, world.getUID, Set.empty)

  def this(location: Location) =
    this(location.getX, location.getY, location.getZ, location.getYaw, location.getPitch, location.getWorld)

  def teleport(player: Player): Boolean =
    location.exists(l => l.getChunk.load(false) && player.teleport(l, TeleportCause.COMMAND))

  def teleportEither[A](player: Player, error: A): Either[A, Unit] =
    Either.cond(teleport(player), (), error)

  def world: Option[World] = Option(Bukkit.getWorld(worldUuid))

  def location: Option[Location] = world.map(new Location(_, x, y, z, yaw, pitch))

  def addResident(resident: UUID): OldHome = copy(residents = residents + resident)

  def removeResident(resident: UUID): OldHome = copy(residents = residents - resident)

  def toHomeK(owner: UUID, name: String)(using config: HSHConfig): HomeK[perspective.Id] =
    val now = Instant.now()
    HomeK(
      owner,
      name,
      now,
      now,
      x,
      y,
      z,
      yaw,
      pitch,
      worldUuid,
      config.serverName
    )
