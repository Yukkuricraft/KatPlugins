package net.katsstuff.bukkit.homesweethome.home

import java.time.{Instant, ZoneOffset}
import java.util.UUID

import dataprism.KMacros
import dataprism.skunk.sql.SkunkTypes.*
import dataprism.sql.*
import io.circe.*
import perspective.*

case class ResidentK[F[_]](
    owner: F[UUID],
    homeName: F[String],
    resident: F[UUID],
    createdAt: F[Instant]
)
type Resident = ResidentK[Id]
inline def Resident: ResidentK.type = ResidentK

object ResidentK {
  given instance[F[_]](
      using uuid: F[UUID],
      string: F[String],
      instant: F[Instant]
  ): ResidentK[F] =
    ResidentK(uuid, string, uuid, instant)

  val table: Table[skunk.Codec, ResidentK] = Table(
    "home_residents",
    ResidentK(
      Column("home_owner", uuid),
      Column("home_name", text),
      Column("resident", uuid),
      Column("created_at", timestamptz.imap(_.toInstant)(_.atOffset(ZoneOffset.UTC)))
    )
  )

  given KMacros.RepresentableTraverseKC[ResidentK] = KMacros.deriveRepresentableTraverseKC[ResidentK]

  given Codec[ResidentK[Id]] with {
    override def apply(c: HCursor): Decoder.Result[ResidentK[Id]] =
      val resDecoderResult: ResidentK[Decoder.Result] = table.columns
        .map2K(instance[Decoder])(
          [X] => (column: Column[skunk.Codec, X], decoder: Decoder[X]) => c.get(column.nameStr)(decoder)
        )
      resDecoderResult.sequenceIdK

    override def apply(a: ResidentK[Id]): Json =
      Json.obj(
        a.tupledK(instance[Encoder])
          .map2Const(table.columns)(
            [X] => (t: (X, Encoder[X]), column: Column[skunk.Codec, X]) => column.nameStr -> t._2(t._1)
          )
          .toListK*
      )
  }
}
