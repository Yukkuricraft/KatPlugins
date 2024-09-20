package net.katsstuff.bukkit.katlib.command

import org.bukkit.command.CommandSender

trait UserValidator[A]:
  def validate(source: CommandSender): Either[CommandFailure, A]
