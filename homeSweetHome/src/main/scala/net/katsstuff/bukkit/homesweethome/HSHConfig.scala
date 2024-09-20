package net.katsstuff.bukkit.homesweethome

import java.nio.file.Files

import scala.jdk.CollectionConverters.*
import scala.util.Try

import io.circe.*
import io.circe.Decoder.Result
import io.circe.yaml.parser
import net.katsstuff.bukkit.homesweethome.HSHConfig.CrossServerCommunication
import net.katsstuff.bukkit.katlib.ScalaPlugin

case class HSHConfig(
    home: HSHConfig.HomeConfig,
    storage: HSHConfig.StorageConfig,
    serverName: String,
    crossServerCommunication: HSHConfig.CrossServerCommunication
) derives Decoder
object HSHConfig:

  case class HomeConfig(
      homeLimit: Int,
      residentLimit: Int,
      timeout: Int
  )

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
    case MultiFile
    case Postgres
  object StorageType:
    given Decoder[StorageType] with
      override def apply(c: HCursor): Decoder.Result[StorageType] = c.as[String].flatMap {
        case "SingleFile" => Right(StorageType.SingleFile)
        case "MultiFile"  => Right(StorageType.MultiFile)
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

  def load()(implicit scalaPlugin: ScalaPlugin): Either[Throwable, HSHConfig] =
    val path = scalaPlugin.dataFolder.toPath.resolve("config.yml")
    scalaPlugin.logger.info("Loading Config")
    Try {
      Files.createDirectories(path.getParent)
      scalaPlugin.saveDefaultConfig()
      Files.readAllLines(path).asScala.mkString("\n")
    }.toEither.flatMap(parser.parse).flatMap { json =>
      val cursor = json.hcursor
      cursor.get[Int]("version").flatMap {
        case 3 => cursor.as[HSHConfig]
        case 2 =>
          scalaPlugin.saveResource("config-v3.yml", false)

          Left(
            new Exception(
              s"Please update the config. The current form of the config has been saved as \"config-v3.yml\""
            )
          )
        case _ => Left(new Exception("Unsupported version"))
      }
    }
