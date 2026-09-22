package net.katsstuff.bukkit.magicalwarps

import scala.io.Source

import io.circe.Json
import io.circe.yaml.parser

class WarpsConfigTest extends munit.FunSuite:

  private def resource(name: String): Json =
    parser.parse(Source.fromResource(name).mkString).toTry.get

  private def withPostgres(json: Json, fields: (String, Json)*): Json =
    json.hcursor
      .downField("storage")
      .downField("postgres")
      .withFocus(_.mapObject(o => fields.foldLeft(o)((o, f) => o.add(f._1, f._2))))
      .top
      .get

  test("the default config can be read") {
    val config = resource("config.yml").as[WarpsConfig].toTry.get
    assertEquals(config.storage.postgres.schema, "magicalwarps")
    assertEquals(resource("config.yml").hcursor.get[Int]("version"), Right(2))
  }

  test("the saved copy of the config is the same as the default config") {
    assertEquals(resource("config-v2.yml"), resource("config.yml"))
  }

  test("using Postgres with the public schema is rejected") {
    val json = withPostgres(resource("config.yml"), "use" -> Json.True, "schema" -> Json.fromString("public"))
    assert(json.as[WarpsConfig].isLeft)
  }

  test("using Postgres with a dedicated schema is accepted") {
    val json   = withPostgres(resource("config.yml"), "use" -> Json.True)
    val config = json.as[WarpsConfig].toTry.get
    assertEquals(config.storage.postgres.connectionParameters, Map("search_path" -> "magicalwarps"))
  }

  test("the schema isn't checked when Postgres isn't used") {
    val json = withPostgres(resource("config.yml"), "use" -> Json.False, "schema" -> Json.fromString("public"))
    assert(json.as[WarpsConfig].isRight)
  }
