package net.katsstuff.bukkit.katlib.db

import java.io.BufferedReader

import scala.jdk.StreamConverters.*

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import dataprism.skunk.sql.SkunkTypes.*
import dataprism.sql.*
import net.katsstuff.bukkit.katlib.ScalaPlugin
import skunk.Codec

object DbUpdates {

  // Solution Play framework uses. Not a fan, but it's simple
  def splitStringIntoSqlStatements(str: String): Seq[String] =
    str.split("(?<!;);(?!;)").view.map(_.trim.replace(";;", ";")).filter(_ != "").toSeq

  private def runDbUpdatesFrom(
      previousVersion: Int,
      presentDbVersion: Int
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
            splitStringIntoSqlStatements(sqlMigration)
              .map(s => SqlStr.const(s))
              .traverse_(tx.run)
          }

          if versionToWrite < presentDbVersion then runMigration *> runDbUpdatesFrom(versionToWrite, presentDbVersion)
          else runMigration
        case None => IO.unit
      }

  private def writeCurrentDbVersion(presentDbVersion: Int)(using db: Db[IO, Codec]): IO[Int] =
    db.run(sql"UPDATE version SET version = (${presentDbVersion.toShort.asArg(int2.codec)})")

  def updateIfNeeded(presentDbVersion: Int)(
      using db: TransactionalDb[IO, Codec],
      plugin: ScalaPlugin
  ): IO[Unit] =
    for
      _       <- db.run(sql"CREATE TABLE IF NOT EXISTS version(version int2)")
      version <- db.runIntoSimple[Short](sql"SELECT version FROM version", int2.codec).map(_.headOption)
      _ <- version match
        case None =>
          for
            _ <- runDbUpdatesFrom(0, presentDbVersion)
            _ <- db.run(sql"INSERT INTO version VALUES (${presentDbVersion.toShort.asArg(int2.codec)})")
          yield ()
        case Some(v) => runDbUpdatesFrom(v, presentDbVersion).flatMap(_ => writeCurrentDbVersion(presentDbVersion))
    yield ()
}
