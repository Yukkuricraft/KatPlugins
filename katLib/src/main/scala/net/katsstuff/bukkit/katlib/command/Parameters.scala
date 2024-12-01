package net.katsstuff.bukkit.katlib.command

import java.time.Duration
import java.util.UUID

import scala.jdk.CollectionConverters.*
import scala.util.Try

import cats.data.{EitherT, State}
import org.bukkit.command.CommandSender
import org.bukkit.entity.Player
import org.bukkit.{Bukkit, OfflinePlayer, World}

object SpecialParameters:
  val manyNamedPlayers: Parameter[Set[Player]] = Parameter.choices(
    "player",
    Bukkit.getOnlinePlayers.asScala.map((p: Player) => p.getName -> p).toMap
  )

  val uuidPlayer: Parameter[Player] = Parameter.choicesSingleMap(
    "player-uuid",
    Bukkit.getOnlinePlayers.asScala.map((p: Player) => p.getUniqueId.toString -> p).toMap,
    showSuggestions = false
  )

  val onlyOfflinePlayers: Parameter[Set[OfflinePlayer]] =
    Parameter
      .choicesSingleOpt(
        "offline-player",
        arg => Option(Bukkit.getOfflinePlayerIfCached(arg))
      )
      .map(Set(_))

  val offlineUuidPlayer: Parameter[OfflinePlayer] = Parameter.choicesSingleOpt(
    "offline-player-uuid",
    arg =>
      Try(UUID.fromString(arg)).toOption
        .map(uuid => Bukkit.getOfflinePlayer(uuid))
        .flatMap(p => Option.when(p.hasPlayedBefore)(p))
  )
end SpecialParameters

type Parser[A] = RawCmdArg => CommandResult[A]

object Parameters:
  def parserFromOption[A](name: String, f: String => Option[A]): Parser[A] = s =>
    f(s.content).toRight(CommandSyntaxError(s"Not a valid $name", s.start))

  val stringParser: Parser[String] = s => Right(s.content)

  val unit: Parameter[Unit] = new SyncParameter[Unit] {
    override def parseSync(source: CommandSender): EitherT[SyncParameterState, CommandFailure, Unit] =
      EitherT.pure[SyncParameterState, CommandFailure](())

    override def suggestionsSync(source: CommandSender): SyncParameterState[List[String]] =
      State.pure(Nil)

    override def usage(source: CommandSender): Usage = Usage.Empty
  }

  val int: Parameter[Int] = Parameter.singleSyncParameter(
    "integer",
    parserFromOption("integer", _.toIntOption)
  )
  val string: Parameter[String] = Parameter
    .singleSyncParameter("string", stringParser)
    .emap(s => if s.isEmpty then Left(CommandFailure.NotEnoughArguments("argument")) else Right(s))
  val double: Parameter[Double] = Parameter.singleSyncParameter(
    "number",
    parserFromOption("number", _.toDoubleOption)
  )
  val uuid: Parameter[UUID] =
    Parameter.singleSyncParameter(
      "uuid",
      parserFromOption("UUID", s => Try(UUID.fromString(s)).toOption)
    )

  val remainingAsString: Parameter[String] = new SyncParameter[String]:
    override def parseSync(source: CommandSender): EitherT[SyncParameterState, CommandFailure, String] =
      EitherT(State(s => (Nil, Right(s.map(_.content).mkString(" ")))))

    override def suggestionsSync(source: CommandSender): SyncParameterState[List[String]] =
      State.set(Nil).map(_ => Nil)

    override def usage(source: CommandSender): Usage = Usage.optional(Usage.Required("strings..."))
  end remainingAsString

  val manyPlayers: Parameter[Set[Player]] =
    (SpecialParameters.manyNamedPlayers | SpecialParameters.uuidPlayer.map(Set(_))).named("player")

  val player: Parameter[Player] =
    (single(SpecialParameters.manyNamedPlayers) | SpecialParameters.uuidPlayer).named("player")

  val offlinePlayers: Parameter[Set[OfflinePlayer]] =
    (manyPlayers.map(_.map(p => p: OfflinePlayer)) |
      SpecialParameters.onlyOfflinePlayers |
      SpecialParameters.offlineUuidPlayer.map(Set(_))).named("user")

  val world: Parameter[World] =
    single(
      Parameter.choices(
        "world",
        Bukkit.getWorlds.asScala.map(w => w.getName -> w).toMap
      )
    )

  val dateTime: LocalDateTimeParameter = new LocalDateTimeParameter(false)

  val duration: Parameter[Duration] = new DurationParameter()
end Parameters
