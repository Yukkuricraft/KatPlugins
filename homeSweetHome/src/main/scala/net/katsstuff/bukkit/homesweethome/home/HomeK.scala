package net.katsstuff.bukkit.homesweethome.home

import java.time.{Instant, ZoneOffset}
import java.util.UUID

import scala.jdk.CollectionConverters.*

import cats.Applicative
import cats.syntax.all.*
import dataprism.KMacros
import dataprism.skunk.sql.SkunkTypes.*
import dataprism.sql.*
import io.circe.*
import net.katsstuff.bukkit.homesweethome.HSHConfig
import net.katsstuff.bukkit.katlib.text.*
import org.bukkit.entity.Player
import org.bukkit.event.player.PlayerTeleportEvent.TeleportCause
import org.bukkit.{Bukkit, Location, World}
import perspective.*

type Home = HomeK[Id]
inline def Home: HomeK.type = HomeK
case class HomeK[F[_]](
    owner: F[UUID],
    name: F[String],
    createdAt: F[Instant],
    updatedAt: F[Instant],
    x: F[Double],
    y: F[Double],
    z: F[Double],
    yaw: F[Float],
    pitch: F[Float],
    worldUuid: F[UUID],
    server: F[String]
)
object HomeK:
  given instance[F[_]](
      using uuid: F[UUID],
      string: F[String],
      instant: F[Instant],
      double: F[Double],
      float: F[Float]
  ): HomeK[F] =
    HomeK(uuid, string, instant, instant, double, double, double, float, float, uuid, string)

  extension (homeK: HomeK[Id])
    def toOldHome(residents: Set[UUID]): OldHome =
      OldHome(homeK.x, homeK.y, homeK.z, homeK.yaw, homeK.pitch, homeK.worldUuid, residents)

    def world: Option[World] = Option(Bukkit.getWorld(homeK.worldUuid))

    def location: Option[Location] = world.map { w =>
      val res = locationWithoutWorld
      res.setWorld(w)
      res
    }

    def locationWithoutWorld = new Location(null, homeK.x, homeK.y, homeK.z, homeK.yaw, homeK.pitch)

  def makeNew(owner: UUID, name: String, x: Double, y: Double, z: Double, yaw: Float, pitch: Float, world: World)(
      using config: HSHConfig
  ): HomeK[Id] =
    val now = Instant.now()
    HomeK(owner, name, now, now, x, y, z, yaw, pitch, world.getUID, config.serverName)

  def makeNew(owner: UUID, name: String, location: Location)(using HSHConfig): HomeK[Id] =
    makeNew(
      owner,
      name,
      location.getX,
      location.getY,
      location.getZ,
      location.getYaw,
      location.getPitch,
      location.getWorld
    )

  val table: Table[skunk.Codec, HomeK] = Table(
    "homes",
    HomeK(
      owner = Column("owner", uuid),
      name = Column("name", text),
      createdAt = Column("created_at", timestamptz.imap(_.toInstant)(_.atOffset(ZoneOffset.UTC))),
      updatedAt = Column("updated_at", timestamptz.imap(_.toInstant)(_.atOffset(ZoneOffset.UTC))),
      x = Column("x", float8),
      y = Column("y", float8),
      z = Column("z", float8),
      yaw = Column("yaw", float4),
      pitch = Column("pitch", float4),
      worldUuid = Column("world_uuid", uuid),
      server = Column("server", text)
    )
  )

  given typeclass: KMacros.RepresentableTraverseKC[HomeK] = KMacros.deriveRepresentableTraverseKC[HomeK]

  given Codec[HomeK[Id]] with
    override def apply(c: HCursor): Decoder.Result[HomeK[Id]] =
      val resDecoderResult: HomeK[Decoder.Result] = table.columns
        .map2K(instance[Decoder])(
          [X] => (column: Column[skunk.Codec, X], decoder: Decoder[X]) => c.get(column.nameStr)(decoder)
        )
      resDecoderResult.sequenceIdK

    override def apply(a: HomeK[Id]): Json =
      Json.obj(
        a.tupledK(instance[Encoder])
          .map2Const(table.columns)(
            [X] => (t: (X, Encoder[X]), column: Column[skunk.Codec, X]) => column.nameStr -> t._2(t._1)
          )
          .toListK*
      )
