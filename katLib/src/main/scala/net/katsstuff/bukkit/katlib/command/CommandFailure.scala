package net.katsstuff.bukkit.katlib.command

import cats.data.NonEmptyList

enum CommandFailure(val shouldShowUsage: Boolean):
  case NotEnoughArguments(forWhat: String)                           extends CommandFailure(true)
  case UnconsumedInput(unconsumed: List[RawCmdArg])                  extends CommandFailure(true)
  case LiteralNotMatched(arg: String, allowed: NonEmptyList[String]) extends CommandFailure(true)
  case CommandError(msgStr: String, override val shouldShowUsage: Boolean = false)
      extends CommandFailure(shouldShowUsage)
  case CommandSyntaxError(msgStr: String, pos: Int) extends CommandFailure(true)
  case CommandUsageError(msgStr: String, pos: Int)  extends CommandFailure(false)

  def msg: String = this match {
    case NotEnoughArguments(forWhat) => s"Missing parameter for $forWhat"
    case UnconsumedInput(unconsumed) => s"Unconsumed input: ${unconsumed.map(_.content).mkString(" ")}"
    case LiteralNotMatched(arg, allowed) =>
      if allowed.tail.isEmpty then s"$arg does not match ${allowed.head}"
      else s"$arg does not match ${allowed.toList.mkString("(", "|", ")")}"
    case CommandError(msg, _)       => msg
    case CommandSyntaxError(msg, _) => msg
    case CommandUsageError(msg, _)  => msg
  }

export CommandFailure.*
