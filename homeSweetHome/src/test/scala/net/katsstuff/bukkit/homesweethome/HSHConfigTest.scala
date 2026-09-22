package net.katsstuff.bukkit.homesweethome

import scala.io.Source

import io.circe.Json
import io.circe.yaml.parser

class HSHConfigTest extends munit.FunSuite:

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
    val config = resource("config.yml").as[HSHConfig].toTry.get
    assertEquals(config.storage.postgres.schema, "homesweethome")
    assertEquals(resource("config.yml").hcursor.get[Int]("version"), Right(4))
  }

  test("the saved copy of the config is the same as the default config") {
    assertEquals(resource("config-v4.yml"), resource("config.yml"))
  }

  test("using Postgres with the public schema is rejected") {
    val json = withPostgres(resource("config.yml"), "use" -> Json.True, "schema" -> Json.fromString("public"))
    assert(json.as[HSHConfig].isLeft)
  }

  test("using Postgres with a conflicting search_path is rejected") {
    val json = withPostgres(
      resource("config.yml"),
      "use"        -> Json.True,
      "parameters" -> Json.obj("search_path" -> Json.fromString("other"))
    )
    assert(json.as[HSHConfig].isLeft)
  }

  test("using Postgres with a dedicated schema is accepted") {
    val json   = withPostgres(resource("config.yml"), "use" -> Json.True)
    val config = json.as[HSHConfig].toTry.get
    assertEquals(config.storage.postgres.connectionParameters, Map("search_path" -> "homesweethome"))
  }

  test("the schema isn't checked when Postgres isn't used") {
    val json = withPostgres(resource("config.yml"), "use" -> Json.False, "schema" -> Json.fromString("public"))
    assert(json.as[HSHConfig].isRight)
  }

  test("the schema is required") {
    val json = resource("config.yml").hcursor.downField("storage").downField("postgres").downField("schema").delete.top.get
    assert(json.as[HSHConfig].isLeft)
  }
