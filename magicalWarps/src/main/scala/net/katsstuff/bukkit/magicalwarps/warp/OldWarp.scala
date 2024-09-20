package net.katsstuff.bukkit.magicalwarps.warp

import java.util.UUID

import io.circe.*
import net.katsstuff.bukkit.katlib.text.*
import net.katsstuff.bukkit.magicalwarps.WarpsConfig
import net.kyori.adventure.text.serializer.legacy.LegacyComponentSerializer

case class OldWarp(
    x: Double,
    y: Double,
    z: Double,
    yaw: Float,
    pitch: Float,
    world: UUID,
    displayName: Option[Text],
    groups: Set[String],
    allowedPermGroups: Set[String],
    allowedUsers: Set[UUID],
    lore: Option[Text]
) derives Codec.AsObject:

  def toWarpK(name: String)(using config: WarpsConfig): Warp = Warp(
    name,
    x,
    y,
    z,
    yaw,
    pitch,
    world,
    config.serverName,
    displayName,
    groups,
    allowedPermGroups,
    allowedUsers,
    lore
  )
object OldWarp:
  implicit val textCodec: Codec[Text] = new Codec[Text]:
    override def apply(a: Text): Json = parser.parse(LegacyComponentSerializer.legacyAmpersand().serialize(a)).toTry.get

    override def apply(c: HCursor): Decoder.Result[Text] = Right(
      LegacyComponentSerializer.legacyAmpersand().deserialize(c.value.noSpaces)
    )
  end textCodec
