package net.katsstuff.bukkit.magicalwarps

import java.nio.file.Files

import scala.jdk.CollectionConverters.*
import scala.util.Try

import io.circe.*
import io.circe.Decoder.Result
import io.circe.yaml.parser
import net.katsstuff.bukkit.katlib.ScalaPlugin

case class WarpsConfig(
    storage: WarpsConfig.StorageConfig,
    serverName: String,
    crossServerCommunication: WarpsConfig.CrossServerCommunication
) derives Decoder
object WarpsConfig:

  case class StorageConfig(`type`: StorageType, postgres: PostgresOptions)

  case class PostgresOptions(
      use: Boolean,
      host: String,
      port: Int,
      user: String,
      database: String,
      password: Option[String],
      maxConnections: Int,
      parameters: Map[String, String]
  )

  enum StorageType:
    case SingleFile
    case Postgres
  object StorageType:
    given Decoder[StorageType] with
      override def apply(c: HCursor): Decoder.Result[StorageType] = c.as[String].flatMap {
        case "SingleFile" => Right(StorageType.SingleFile)
        case "Postgres"   => Right(StorageType.Postgres)
        case other        => Left(DecodingFailure(s"Unknown storage type $other", c.history))
      }

  enum CrossServerCommunication:
    case Postgres
    case Single
  object CrossServerCommunication:
    given Decoder[CrossServerCommunication] with
      override def apply(c: HCursor): Result[CrossServerCommunication] = c.as[String].flatMap {
        case "Postgres" => Right(CrossServerCommunication.Postgres)
        case "Single"   => Right(CrossServerCommunication.Single)
        case other      => Left(DecodingFailure(s"Unknown cross server communication method $other", c.history))
      }

  def load()(implicit scalaPlugin: ScalaPlugin): Either[Throwable, WarpsConfig] =
    val path = scalaPlugin.dataFolder.toPath.resolve("config.yml")
    scalaPlugin.logger.info("Loading Config")
    Try {
      Files.createDirectories(path.getParent)
      scalaPlugin.saveDefaultConfig()
      Files.readAllLines(path).asScala.mkString("\n")
    }.toEither.flatMap(parser.parse).flatMap { json =>
      val cursor = json.hcursor
      cursor.get[Int]("version").flatMap {
        case 1 => cursor.as[WarpsConfig]
        case _ => Left(new Exception("Unsupported version"))
      }
    }
