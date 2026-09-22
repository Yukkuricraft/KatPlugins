package net.katsstuff.bukkit.katlib.db

import java.util.Locale

object PostgresSchema:

  private val SearchPath = "search_path"

  private def normalize(schema: String): String =
    schema.trim.stripPrefix("\"").stripSuffix("\"").toLowerCase(Locale.ROOT)

  /** Checks that a plugin has its own schema. */
  def validate(schema: String, parameters: Map[String, String]): Either[String, Unit] =
    if schema.trim.isEmpty then Left("The Postgres schema can not be empty")
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
