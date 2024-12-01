package net.katsstuff.bukkit.katlib.brigcommands

import scala.util.boundary.{Label, break}

import com.mojang.brigadier.context.CommandContext
import org.bukkit.entity.Player

inline def cmdError(error: String)(using Label[CommandResult.Error]): Nothing =
  break(CommandResult.Error(error): CommandResult.Error)

inline def useCtx()(using ctx: CommandContext[Source]): ctx.type = ctx

inline def useSource()(using ctx: CommandContext[Source]) = ctx.getSource

inline def useSender()(using ctx: CommandContext[Source]) = ctx.getSource.getSender

inline def usePlayerSender()(using ctx: CommandContext[Source])(using Label[CommandResult.Error]): Player =
  useSource().getSender match
    case p: Player => p
    case _         => cmdError("Only players can use this command")

inline def usePlayerExecutor()(using ctx: CommandContext[Source])(using Label[CommandResult.Error]): Player =
  useSource().getExecutor match
    case p: Player => p
    case _         => cmdError("Only players can use this command")

extension [A](opt: Option[A])
  inline def getOrError(error: String)(using Label[CommandResult.Error]): A = opt match
    case Some(value) => value
    case None        => cmdError(error)

extension [A](either: Either[String, A])
  inline def getOrError(using Label[CommandResult.Error]): A = either match
    case Right(value) => value
    case Left(err)    => cmdError(err)
