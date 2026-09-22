package net.katsstuff.bukkit.katlib.db

class PostgresSchemaTest extends munit.FunSuite:

  test("a dedicated schema is accepted") {
    assertEquals(PostgresSchema.validate("homesweethome", Map.empty), Right(()))
  }

  test("an empty schema is rejected") {
    assert(PostgresSchema.validate("", Map.empty).isLeft)
    assert(PostgresSchema.validate("   ", Map.empty).isLeft)
  }

  test("public is rejected however it is written") {
    for schema <- Seq("public", "PUBLIC", "Public", " public ", "\"public\"") do
      assert(PostgresSchema.validate(schema, Map.empty).isLeft, schema)
  }

  test("schemas must be identifiers") {
    for schema <- Seq("with space", "semi;colon", "1digit", "quote'", "\"inner\"quote\"", "\"\"") do
      assert(PostgresSchema.validate(schema, Map.empty).isLeft, schema)
    for schema <- Seq("plugin_data", "Plugin$1", "_x", "\"Any Thing\"") do
      assertEquals(PostgresSchema.validate(schema, Map.empty), Right(()), schema)
  }

  test("names are folded like Postgres does") {
    assertEquals(PostgresSchema.name("MixedCase"), "mixedcase")
    assertEquals(PostgresSchema.name(" \"MixedCase\" "), "MixedCase")
    assertEquals(PostgresSchema.identifier("MixedCase"), "\"mixedcase\"")
    assertEquals(PostgresSchema.identifier("\"Mixed Case\""), "\"Mixed Case\"")
  }

  test("a search_path parameter matching the schema is accepted") {
    assertEquals(PostgresSchema.validate("warps", Map("search_path" -> "warps")), Right(()))
    assertEquals(PostgresSchema.validate("warps", Map("search_path" -> "WARPS")), Right(()))
  }

  test("a search_path parameter conflicting with the schema is rejected") {
    assert(PostgresSchema.validate("warps", Map("search_path" -> "public")).isLeft)
    assert(PostgresSchema.validate("warps", Map("search_path" -> "homes")).isLeft)
  }

  test("other parameters are left alone") {
    assertEquals(PostgresSchema.validate("warps", Map("application_name" -> "foo")), Right(()))
  }

  test("connectionParameters sets the search_path to the schema") {
    assertEquals(
      PostgresSchema.connectionParameters("warps", Map("application_name" -> "foo")),
      Map("application_name" -> "foo", "search_path" -> "warps")
    )
    assertEquals(
      PostgresSchema.connectionParameters("warps", Map("search_path" -> "warps")),
      Map("search_path" -> "warps")
    )
  }
