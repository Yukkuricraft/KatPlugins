package net.katsstuff.bukkit.magicalwarps.warp

import java.util.UUID

import cats.syntax.all.*
import dataprism.KMacros
import dataprism.skunk.sql.SkunkTypes.*
import dataprism.sql.*
import io.circe.*
import io.circe.Decoder.Result
import net.katsstuff.bukkit.katlib.text.*
import net.katsstuff.bukkit.magicalwarps.WarpsConfig
import net.kyori.adventure.text.serializer.json.JSONComponentSerializer
import net.kyori.adventure.text.serializer.plain.PlainTextComponentSerializer
import org.bukkit.{Bukkit, Location}
import perspective.Id
import skunk.data.Arr

type Warp = WarpK[Id]
inline def Warp: WarpK.type = WarpK
case class WarpK[F[_]](
    name: F[String],
    x: F[Double],
    y: F[Double],
    z: F[Double],
    yaw: F[Float],
    pitch: F[Float],
    world: F[UUID],
    server: F[String],
    displayName: F[Option[Text]],
    groups: F[Seq[String]],
    allowedPermGroups: F[Seq[String]],
    allowedUsers: F[Seq[UUID]],
    lore: F[Option[Text]]
)

object WarpK:
  given instance[F[_]](
      using string: F[String],
      double: F[Double],
      float: F[Float],
      uuid: F[UUID],
      optText: F[Option[Text]],
      setString: F[Seq[String]],
      setUuid: F[Seq[UUID]]
  ): WarpK[F] =
    WarpK(string, double, double, double, float, float, uuid, string, optText, setString, setString, setUuid, optText)

  def fromLocation(name: String, location: Location)(using config: WarpsConfig): WarpK[Id] =
    WarpK(
      name,
      location.getX,
      location.getY,
      location.getZ,
      location.getYaw,
      location.getPitch,
      location.getWorld.getUID,
      config.serverName,
      None,
      Seq.empty,
      Seq.empty,
      Seq.empty,
      None
    )

  extension (warpId: WarpK[Id])
    def getLocation: Option[Location] =
      Option(Bukkit.getWorld(warpId.world)).map { w =>
        val l = locationWithoutWorld
        l.setWorld(w)
        l
      }

    def locationWithoutWorld: Location = new Location(null, warpId.x, warpId.y, warpId.z, warpId.yaw, warpId.pitch)

    def stringDisplayName: String =
      warpId.displayName.map(PlainTextComponentSerializer.plainText().serialize).getOrElse(warpId.name.capitalize)
    def textDisplayName: Text = warpId.displayName.getOrElse(t"${warpId.name.capitalize}")

  private val mcTextDbType = {
    val serializer = JSONComponentSerializer.json()

    skunk.circe.codec.json.jsonb
      .imap(v => serializer.deserialize(v.noSpaces))(c => io.circe.parser.parse(serializer.serialize(c)).toTry.get)
      .wrap
  }

  val table: Table[skunk.Codec, WarpK] = Table(
    "warps",
    WarpK(
      name = Column("name", text),
      x = Column("x", float8),
      y = Column("y", float8),
      z = Column("z", float8),
      yaw = Column("yaw", float4),
      pitch = Column("pitch", float4),
      world = Column("world_uuid", uuid),
      server = Column("server", text),
      displayName = Column("display_name", mcTextDbType.nullable),
      groups = Column("groups", _text),
      allowedPermGroups = Column("allowed_perm_groups", _text),
      allowedUsers = Column("allowed_users", arrayOf(uuid)),
      lore = Column("lore", mcTextDbType.nullable)
    )
  )

  given typeclass: KMacros.RepresentableTraverseKC[WarpK] = KMacros.deriveRepresentableTraverseKC[WarpK]

  given Codec[WarpK[Id]] with
    given textCodec: Codec[Text] = new Codec[Text]:
      override def apply(a: Text): Json = parser.parse(JSONComponentSerializer.json().serialize(a)).toTry.get

      override def apply(c: HCursor): Result[Text] = Right(JSONComponentSerializer.json().deserialize(c.value.noSpaces))
    end textCodec

    override def apply(c: HCursor): Decoder.Result[WarpK[Id]] =
      val resDecoderResult: WarpK[Decoder.Result] = table.columns
        .map2K(instance[Decoder])(
          [X] => (column: Column[skunk.Codec, X], decoder: Decoder[X]) => c.get(column.nameStr)(decoder)
        )
      resDecoderResult.sequenceIdK

    override def apply(a: WarpK[Id]): Json =
      Json.obj(
        a.tupledK(instance[Encoder])
          .map2Const(table.columns)(
            [X] => (t: (X, Encoder[X]), column: Column[skunk.Codec, X]) => column.nameStr -> t._2(t._1)
          )
          .toListK*
      )
