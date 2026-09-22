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

  /**
    * Advisory lock held while applying updates. Advisory locks are per
    * database, and every plugin uses the same key, so only one server and
    * plugin updates a database at a time.
    */
  private val UpdateLockKey: Long = 0x4B61744C6962L // "KatLib"

  // Solution Play framework uses. Not a fan, but it's simple
  def splitStringIntoSqlStatements(str: String): Seq[String] =
    str.split("(?<!;);(?!;)").view.map(_.trim.replace(";;", ";")).filter(_ != "").toSeq

  private def readMigration(version: Int)(using plugin: ScalaPlugin): IO[String] =
    Resource
      .make(IO(Option(plugin.textResource(s"db-updates/$version.sql")))) {
        case Some(value) => IO(value.close())
        case None        => IO.unit
      }
      .map(_.map(new BufferedReader(_)))
      .use(_.traverse(r => IO.blocking(r.lines().toScala(Seq).mkString("\n"))))
      .flatMap {
        case Some(sqlMigration) => IO.pure(sqlMigration)
        case None               => IO.raiseError(new Exception(s"Missing database update db-updates/$version.sql"))
      }

  /**
    * Brings the database up to presentDbVersion. Everything happens in one
    * transaction, so a failed update leaves the database as it was.
    */
  def updateIfNeeded(presentDbVersion: Int)(
      using db: TransactionalDb[IO, Codec],
      plugin: ScalaPlugin
  ): IO[Unit] =
    updateIfNeeded(presentDbVersion, readMigration, message => plugin.logger.info(message))

  private[db] def updateIfNeeded(presentDbVersion: Int, readMigration: Int => IO[String], log: String => Unit)(
      using db: TransactionalDb[IO, Codec]
  ): IO[Unit] =
    db.transaction { (tx: TransactionDb[IO, Codec]) ?=>
      for
        // Released automatically when the transaction ends. Parameters are not supported in the
        // transaction, so the (constant) values are inlined
        _ <- tx.runIntoSimple[Int](SqlStr.const(s"SELECT 1 FROM pg_advisory_xact_lock($UpdateLockKey)"), int4.codec)
        _ <- tx.run(sql"CREATE TABLE IF NOT EXISTS version(version int2)")
        version <- tx.runIntoSimple[Short](sql"SELECT version FROM version", int2.codec).map(_.headOption)
        currentVersion = version.fold(0)(_.toInt)
        _ <-
          if currentVersion > presentDbVersion then
            IO.raiseError(
              new Exception(
                s"The database is at version $currentVersion, which is newer than this plugin supports ($presentDbVersion)"
              )
            )
          else IO.unit
        _ <- (currentVersion + 1 to presentDbVersion).toList.traverse_ { updateVersion =>
          IO(log(s"Applying database update $updateVersion")) *>
            readMigration(updateVersion).flatMap { sqlMigration =>
              splitStringIntoSqlStatements(sqlMigration).map(s => SqlStr.const(s)).traverse_(tx.run)
            }
        }
        _ <- version match
          case None =>
            tx.run(SqlStr.const(s"INSERT INTO version VALUES ($presentDbVersion)"))
          case Some(v) if v != presentDbVersion =>
            tx.run(SqlStr.const(s"UPDATE version SET version = $presentDbVersion"))
          case Some(_) => IO.pure(0)
      yield ()
    }
}
