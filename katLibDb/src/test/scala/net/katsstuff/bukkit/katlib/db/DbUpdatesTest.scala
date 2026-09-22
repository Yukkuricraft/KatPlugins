package net.katsstuff.bukkit.katlib.db

import java.util.concurrent.CopyOnWriteArrayList

import scala.jdk.CollectionConverters.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import dataprism.skunk.sql.SkunkSessionPoolDb
import net.katsstuff.bukkit.katlib.db.testing.TestPostgres
import net.katsstuff.bukkit.katlib.db.testing.TestPostgres.TestDatabase
import skunk.codec.all.*
import skunk.implicits.*

class DbUpdatesTest extends munit.FunSuite:

  private def migrations(sqls: String*): Int => IO[String] =
    version =>
      sqls.lift(version - 1) match
        case Some(sql) => IO.pure(sql)
        case None      => IO.raiseError(new Exception(s"Missing database update $version"))

  private def update(database: TestDatabase, version: Int, readMigration: Int => IO[String]): IO[Unit] =
    DbUpdates.updateIfNeeded(version, database.schema, readMigration, _ => ())(
      using SkunkSessionPoolDb[IO](database.single)
    )

  private val threeMigrations = migrations(
    "CREATE TABLE a(i int)",
    "CREATE TABLE b(i int); ALTER TABLE a ADD COLUMN j int",
    "CREATE TABLE c(i int)"
  )

  test("statements are split on semicolons") {
    assertEquals(DbUpdates.splitStringIntoSqlStatements("SELECT 1; SELECT 2;"), Seq("SELECT 1", "SELECT 2"))
  }

  test("double semicolons are kept as single semicolons in a statement") {
    assertEquals(
      DbUpdates.splitStringIntoSqlStatements("BEGIN foo;; bar;; END; SELECT 1"),
      Seq("BEGIN foo; bar; END", "SELECT 1")
    )
  }

  test("empty statements are dropped") {
    assertEquals(DbUpdates.splitStringIntoSqlStatements(" ; \n;SELECT 1;\n\n"), Seq("SELECT 1"))
    assertEquals(DbUpdates.splitStringIntoSqlStatements(""), Seq())
  }

  test("a fresh database gets all updates and the version") {
    val database = TestPostgres.freshDatabase()
    update(database, 3, threeMigrations).unsafeRunSync()
    assertEquals(database.tables, Set("version", "a", "b", "c"))
    assertEquals(database.version, Some(3.toShort))
  }

  test("tables are created in the schema, and not in public") {
    val database = TestPostgres.freshDatabase(schema = "my_plugin")
    update(database, 1, threeMigrations).unsafeRunSync()
    val schemas = database.query(
      sql"SELECT table_schema::text FROM information_schema.tables WHERE table_name = 'a'".query(text)
    )
    assertEquals(schemas, List("my_plugin"))
  }

  test("the schema is created if it doesn't exist") {
    val database = TestPostgres.freshDatabase(schema = "new_schema", createSchema = false)
    update(database, 1, threeMigrations).unsafeRunSync()
    assertEquals(database.tables, Set("version", "a"))
    assertEquals(database.query(sql"SELECT current_schema()::text".query(text)), List("new_schema"))
  }

  test("a schema with capitals is created the way Postgres reads it") {
    for (schema, created) <- Seq("MixedCase" -> "mixedcase", "\"QuotedCase\"" -> "QuotedCase") do
      val database = TestPostgres.freshDatabase(schema = schema, createSchema = false)
      update(database, 1, threeMigrations).unsafeRunSync()
      val schemas = database.query(
        sql"SELECT table_schema::text FROM information_schema.tables WHERE table_name = 'a'".query(text)
      )
      assertEquals(schemas, List(created), schema)
      // Running again finds the schema
      update(database, 2, threeMigrations).unsafeRunSync()
  }

  test("an existing schema is used without needing permission to create schemas") {
    val admin = TestPostgres.freshDatabase(schema = "limited_schema", createSchema = false)
    val role  = s"limited_${admin.name}"
    admin.executeSql(
      s"""CREATE ROLE $role LOGIN PASSWORD 'limited';
         |CREATE SCHEMA limited_schema AUTHORIZATION $role;
         |REVOKE CREATE ON DATABASE ${admin.name} FROM PUBLIC""".stripMargin
    )
    val limited = admin.copy(login = Some(role -> "limited"))

    update(limited, 1, threeMigrations).unsafeRunSync()
    assertEquals(limited.tables, Set("version", "a"))

    // And without the schema, it fails for lack of permission
    val noSchema = limited.copy(schema = "other_schema")
    intercept[Exception](update(noSchema, 1, threeMigrations).unsafeRunSync())
  }

  test("an invalid schema is rejected before touching the database") {
    val database = TestPostgres.freshDatabase()
    for schema <- Seq("public", "a; DROP TABLE a", "\"quote\"\"\"") do
      intercept[IllegalArgumentException](update(database.copy(schema = schema), 1, threeMigrations).unsafeRunSync())
  }

  test("only the updates after the current version are applied") {
    val database = TestPostgres.freshDatabase()
    update(database, 1, threeMigrations).unsafeRunSync()
    assertEquals(database.tables, Set("version", "a"))
    assertEquals(database.version, Some(1.toShort))

    val applied = new CopyOnWriteArrayList[Int]()
    update(database, 3, v => IO(applied.add(v)) *> threeMigrations(v)).unsafeRunSync()
    assertEquals(applied.asScala.toSeq, Seq(2, 3))
    assertEquals(database.tables, Set("version", "a", "b", "c"))
    assertEquals(database.version, Some(3.toShort))
  }

  test("running again at the same version does nothing") {
    val database = TestPostgres.freshDatabase()
    update(database, 3, threeMigrations).unsafeRunSync()
    update(database, 3, v => IO.raiseError(new Exception(s"Should not read update $v"))).unsafeRunSync()
    assertEquals(database.version, Some(3.toShort))
  }

  test("a failing update leaves the database as it was") {
    val database = TestPostgres.freshDatabase()
    update(database, 1, threeMigrations).unsafeRunSync()

    val failing = migrations("CREATE TABLE a(i int)", "CREATE TABLE b(i int)", "THIS IS NOT SQL")
    intercept[Exception](update(database, 3, failing).unsafeRunSync())

    assertEquals(database.tables, Set("version", "a"))
    assertEquals(database.version, Some(1.toShort))
  }

  test("a failing first update leaves no version table") {
    val database = TestPostgres.freshDatabase()
    intercept[Exception](update(database, 1, migrations("THIS IS NOT SQL")).unsafeRunSync())
    assertEquals(database.tables, Set.empty[String])
  }

  test("a missing update fails") {
    val database = TestPostgres.freshDatabase()
    intercept[Exception](update(database, 4, threeMigrations).unsafeRunSync())
    assertEquals(database.tables, Set.empty[String])
  }

  test("a database newer than the plugin is rejected") {
    val database = TestPostgres.freshDatabase()
    update(database, 3, threeMigrations).unsafeRunSync()
    val e = intercept[Exception](update(database, 2, threeMigrations).unsafeRunSync())
    assert(e.getMessage.contains("newer"), e.getMessage)
    assertEquals(database.version, Some(3.toShort))
  }

  test("functions with semicolons can be created") {
    val database = TestPostgres.freshDatabase()
    update(database, 1, migrations(testing.NotifySql.notifyTriggerFunction)).unsafeRunSync()
    val functions = database.query(
      sql"SELECT proname::text FROM pg_proc WHERE proname = 'notify_trigger'".query(text)
    )
    assertEquals(functions, List("notify_trigger"))
  }

  test("concurrent updates are only applied once, and create the schema once") {
    val database = TestPostgres.freshDatabase(createSchema = false)
    // Not idempotent, so applying it twice fails
    val counting = migrations("CREATE TABLE applied(i int); INSERT INTO applied VALUES (1)", "INSERT INTO applied VALUES (2)")

    (0 until 8).toList.parTraverse_(_ => update(database, 2, counting)).unsafeRunSync()

    assertEquals(database.query(sql"SELECT i FROM applied ORDER BY i".query(int4)), List(1, 2))
    assertEquals(database.version, Some(2.toShort))
  }

  test("plugins with their own schemas in the same database update independently") {
    val homes = TestPostgres.freshDatabase(schema = "homes", createSchema = false)
    val warps = homes.copy(schema = "warps")

    update(homes, 2, threeMigrations).unsafeRunSync()
    update(warps, 1, migrations("CREATE TABLE a(i int)")).unsafeRunSync()

    assertEquals(homes.version, Some(2.toShort))
    assertEquals(warps.version, Some(1.toShort))
    assertEquals(homes.tables, Set("version", "a", "b"))
    assertEquals(warps.tables, Set("version", "a"))
  }
