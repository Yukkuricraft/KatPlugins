package net.katsstuff.bukkit.katlib.command

import cats.data.{EitherT, State}

import java.time.Duration
import cats.syntax.all.*
import org.bukkit.command.CommandSender

//noinspection UnstableApiUsage
class DurationParameter(val name: String = "duration") extends SyncParameter[Duration], NameableParameter[Duration] {
  param =>
  private val durationRegex = """(?:(\d+)[dD])?(?:(\d+)[hH])?(?:(\d+)[mM])?(?:(\d+)[sS])?""".r

  def parse(content: String, start: Int): Either[CommandFailure, Duration] = {
    def combineAcc(current: Option[Int], matched: Option[String]): Either[CommandFailure, Option[Int]] =
      if (current.toSeq ++ matched.toSeq).length == 2 then
        Left(CommandSyntaxError("Specified a duration type twice", start))
      else Right(current.orElse(matched.map(_.toInt)))

    // Find all matches to allow the options to be specified in any order. If an option is specified twice, we error
    if durationRegex.findFirstMatchIn(content).isEmpty then Left(CommandSyntaxError("Invalid duration", start))
    else
      val decodedDurationArgs = durationRegex
        .findAllMatchIn(content)
        .foldLeft[Either[CommandFailure, (Option[Int], Option[Int], Option[Int], Option[Int])]](
          Right((None, None, None, None))
        ) {
          case (Right((accDays, accHours, accMinutes, accSeconds)), matchRes) =>
            (
              combineAcc(accDays, Option(matchRes.group(1))),
              combineAcc(accHours, Option(matchRes.group(2))),
              combineAcc(accMinutes, Option(matchRes.group(3))),
              combineAcc(accSeconds, Option(matchRes.group(4)))
            ).tupled
          case (Left(e), _) => Left(e)
        }
      val duration = decodedDurationArgs.map((days, hours, minutes, seconds) =>
        Duration
          .ofDays(days.getOrElse(0))
          .plusHours(hours.getOrElse(0))
          .plusMinutes(minutes.getOrElse(0))
          .plusSeconds(seconds.getOrElse(0))
      )

      duration
  }

  override def parseSync(source: CommandSender): EitherT[SyncParameterState, CommandFailure, Duration] =
    EitherT(State {
      case Nil => (Nil, Left(CommandSyntaxError("Missing parameter for duration", -1)))
      case x :: xs =>
        val res = parse(x.content, x.start)
        if res.isLeft then (x :: xs, res) else (xs, res)
    })

  override def suggestionsSync(source: CommandSender): SyncParameterState[List[String]] =
    State.set(Nil).map(_ => Nil)

  override def usage(source: CommandSender): Usage = Usage.Required(name)

  override def named(name: String): NameableParameter[Duration] = new DurationParameter(name)
}
