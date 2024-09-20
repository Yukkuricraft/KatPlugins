package net.katsstuff.bukkit.katlib.command

import java.time.format.DateTimeParseException
import java.time.{LocalDate, LocalDateTime, LocalTime}

import scala.util.Try

import cats.data.{EitherT, State}
import org.bukkit.command.CommandSender

class LocalDateTimeParameter(useNowIfMissing: Boolean, val name: String = "dateTime")
    extends SyncParameter[LocalDateTime],
      NameableParameter[LocalDateTime]:
  param =>

  private def parse(s: Option[(String, Int)]): (Boolean, Either[CommandFailure, LocalDateTime]) =
    s
      .map { case (content, start) =>
        Try(LocalDateTime.parse(content))
          .recoverWith { case _: DateTimeParseException =>
            Try(LocalDateTime.of(LocalDate.now, LocalTime.parse(content)))
          }
          .recoverWith { case _: DateTimeParseException =>
            Try(LocalDateTime.of(LocalDate.parse(content), LocalTime.MIDNIGHT))
          }
          .toOption
          .map(res => (true, Right(res)))
          .getOrElse(
            // We handle this as an optional parameter, and recover from the error
            if useNowIfMissing then (false, Right(LocalDateTime.now()))
            else (false, Left(CommandSyntaxError("Invalid date-time", start)))
          )
      }
      .getOrElse(
        if useNowIfMissing then (false, Right(LocalDateTime.now()))
        else (false, Left(CommandSyntaxError("Missing date-time", -1)))
      )

  override def parseSync(source: CommandSender): EitherT[SyncParameterState, CommandFailure, LocalDateTime] =
    EitherT(State { s =>
      val (consumed, res) = parse(s.headOption.map(h => (h.content, h.start)))
      if consumed then (s.drop(1), res) else (s, res)
    })

  override def suggestionsSync(source: CommandSender): SyncParameterState[List[String]] =
    State { input =>
      val arg        = input.headOption.fold("")(_.content)
      val date       = LocalDateTime.now().withNano(0).toString
      val suggestion = if (date.startsWith(arg)) List(date) else Nil
      (input.drop(1), suggestion)
    }

  override def usage(source: CommandSender): Usage =
    val base = Usage.Required(name)
    if useNowIfMissing then Usage.optional(base) else base

  def orNow: Parameter[LocalDateTime] = new LocalDateTimeParameter(true, name)

  override def named(name: String): NameableParameter[LocalDateTime] = new LocalDateTimeParameter(useNowIfMissing, name)
