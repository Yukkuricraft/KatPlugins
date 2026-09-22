package net.katsstuff.bukkit.katlib.db

import java.util.Locale

object PostgresSchema:

  private val SearchPath = "search_path"

  private val UnquotedIdentifier = "[A-Za-z_][A-Za-z0-9_$]*".r
  private val QuotedIdentifier   = "\"[^\"']+\"".r

  private def normalize(schema: String): String =
    schema.trim.stripPrefix("\"").stripSuffix("\"").toLowerCase(Locale.ROOT)

  /** Checks that a plugin has its own schema. */
  def validate(schema: String, parameters: Map[String, String]): Either[String, Unit] =
    if schema.trim.isEmpty then Left("The Postgres schema can not be empty")
    else if !UnquotedIdentifier.matches(schema.trim) && !QuotedIdentifier.matches(schema.trim) then
      Left(s"The Postgres schema ($schema) must be a name made of letters, digits and underscores, or be quoted")
    else if normalize(schema) == "public" then
      Left("The Postgres schema can not be public. Use a dedicated schema for this plugin")
    else
      parameters.get(SearchPath) match
        case Some(searchPath) if normalize(searchPath) != normalize(schema) =>
          Left(s"The Postgres parameter $SearchPath ($searchPath) conflicts with the schema ($schema). Remove it")
        case _ => Right(())

  /** The connection parameters, with the schema as the search path. */
  def connectionParameters(schema: String, parameters: Map[String, String]): Map[String, String] =
    parameters.updated(SearchPath, schema)

  def name(schema: String): String =
    val trimmed = schema.trim
    if trimmed.startsWith("\"") then trimmed.stripPrefix("\"").stripSuffix("\"")
    else trimmed.toLowerCase(Locale.ROOT)

  /** The schema as a quoted identifier, for use in SQL. The schema must be valid. */
  def identifier(schema: String): String = s"\"${name(schema)}\""
