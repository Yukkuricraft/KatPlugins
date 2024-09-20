package net.katsstuff.bukkit.katlib.command

import cats.syntax.all.*
import org.bukkit.command.CommandSender
import org.bukkit.entity.Player

object Senders:

  val commandSender: UserValidator[CommandSender] = (source: CommandSender) => Right(source)

  val player: UserValidator[Player] = {
    case p: Player => Right(p)
    case _         => Left(CommandUsageError("Only players can use this command", -1))
  }
