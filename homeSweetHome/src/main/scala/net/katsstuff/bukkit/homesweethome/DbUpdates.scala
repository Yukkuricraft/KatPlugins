package net.katsstuff.bukkit.homesweethome

import java.io.BufferedReader

import scala.jdk.StreamConverters.*

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import dataprism.skunk.sql.SkunkTypes.*
import dataprism.sql.*
import net.katsstuff.bukkit.katlib.ScalaPlugin
import skunk.util.Origin
import skunk.{Codec, Command, Session}

object DbUpdates {

  private val currentDbVersion: Int = 1

  private def runDbUpdatesFrom(
      previousVersion: Int
  )(using db: TransactionalDb[IO, Codec], plugin: ScalaPlugin): IO[Unit] =
    val versionToWrite = previousVersion + 1
    Resource
      .make(IO(Option(plugin.textResource(s"db-updates/$versionToWrite.sql")))) {
        case Some(value) => IO(value.close())
        case None        => IO.unit
      }
      .map(_.map(new BufferedReader(_)))
      .use(_.traverse(r => IO.blocking(r.lines().toScala(Seq).mkString("\n"))))
      .flatMap {
        case Some(sqlMigration) =>
          val runMigration = db.transaction { (tx: TransactionDb[IO, Codec]) ?=>
            sqlMigration
              .split(";")
              .toSeq
              .map(s => SqlStr.const(s))
              .traverse_(tx.run)
          }

          if versionToWrite < currentDbVersion then runMigration *> runDbUpdatesFrom(versionToWrite)
          else runMigration
        case None => IO.unit
      }

  private def writeCurrentDbVersion()(using db: Db[IO, Codec]): IO[Int] =
    db.run(sql"UPDATE version SET version = (${currentDbVersion.toShort.asArg(int2.codec)})")

  def updateIfNeeded()(
      using db: TransactionalDb[IO, Codec],
      plugin: ScalaPlugin
  ): IO[Unit] =
    for
      _       <- db.run(sql"CREATE TABLE IF NOT EXISTS version(version int2)")
      version <- db.runIntoSimple[Short](sql"SELECT version FROM version", int2.codec).map(_.headOption)
      _ <- version match
        case None =>
          for
            _ <- runDbUpdatesFrom(0)
            _ <- db.run(sql"INSERT INTO version VALUES (${currentDbVersion.toShort.asArg(int2.codec)})")
          yield ()
        case Some(v) => runDbUpdatesFrom(v).flatMap(_ => writeCurrentDbVersion())
    yield ()
}
