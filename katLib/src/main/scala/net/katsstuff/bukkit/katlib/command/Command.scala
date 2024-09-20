package net.katsstuff.bukkit.katlib.command

import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext}
import scala.jdk.CollectionConverters.*
import cats.data.{NonEmptyList, Validated}
import cats.syntax.all.*
import com.destroystokyo.paper.event.server.AsyncTabCompleteEvent
import info.debatty.java.stringsimilarity.RatcliffObershelp
import io.papermc.paper.command.brigadier.Commands
import net.katsstuff.bukkit.katlib.ScalaPlugin
import net.katsstuff.bukkit.katlib.text.*
import net.katsstuff.bukkit.katlib.util.FutureOrNow
import net.kyori.adventure.text.Component
import org.bukkit.Bukkit
import org.bukkit.command.{CommandExecutor, CommandSender, Command as BukkitCommand}
import org.bukkit.event.{EventHandler, Listener}

class Command(val names: Seq[String], val permission: Option[String], val executions: Executions)(
    using ExecutionContext
) extends BukkitCommand(names.head, "", "", names.tail.asJava),
      CommandExecutor,
      Listener:
  override def onCommand(
      source: CommandSender,
      command: BukkitCommand,
      label: String,
      args: Array[String]
  ): Boolean = {
    val res = executions.handleCommandAsync(source, RawCmdArg.stringToRawArgsQuoted(args.mkString(" ")))
    res.foreach {
      case Validated.Valid(_) =>
      case Validated.Invalid(e) =>
        val mostParsed = e.map(_._1).maximum
        val errors     = e.filter(_._1 == mostParsed).map(t => (t._2, t._3)).groupMapReduce(_._1)(_._2)(Usage.merge)
        val errorsWithoutUnconsumed =
          if errors.size > 1
          then
            errors.filter {
              case (CommandFailure.UnconsumedInput(_), _) => false
              case _                                      => true
            }
          else errors

        val similarityComparer = new RatcliffObershelp

        val closestLiteralError = errorsWithoutUnconsumed
          .collect { case (e @ CommandFailure.LiteralNotMatched(arg, allowed), u) =>
            val similarity = allowed.map(similarityComparer.similarity(arg, _)).foldLeft(0D)(_.max(_))
            (e, (u, similarity))
          }
          .filter(_._2._2 > 0.6)
          .map(t => (t._1, t._2._1))

        lazy val (knownSyntaxErrors, otherErrors) = errorsWithoutUnconsumed.partitionMap {
          case (CommandFailure.NotEnoughArguments(_), u)   => Left(u)
          case (CommandFailure.LiteralNotMatched(_, _), u) => Left(u)
          case t                                           => Right(t)
        }

        def printErrorUsage(error: CommandFailure, usage: Usage): String =
          val msg = error.msg

          if error.shouldShowUsage
          then msg + s"\nUsage: ${usage.printUsage}"
          else msg

        val errorsWithUsage =
          if errors.size > 1 && (closestLiteralError.nonEmpty || knownSyntaxErrors.nonEmpty)
          then
            if closestLiteralError.nonEmpty
            then closestLiteralError.map(printErrorUsage)
            else
              printErrorUsage(
                CommandFailure.CommandSyntaxError("Unknown usage", -1),
                knownSyntaxErrors.reduce(Usage.merge)
              ) +: otherErrors.toSeq.map(printErrorUsage)
          else errors.map(printErrorUsage)

        source.sendMessage(t"$Red${errorsWithUsage.mkString("\n\n")}")
    }

    res.asFuture.failed.foreach { e =>
      source.sendMessage(t"${Red}Error: ${e.getMessage}")
      e.printStackTrace()
    }

    true
  }

  override def execute(sender: CommandSender, commandLabel: String, args: Array[String]): Boolean =
    onCommand(sender, this, commandLabel, args)

  @EventHandler(ignoreCancelled = true)
  def onAsyncTabComplete(event: AsyncTabCompleteEvent): Unit =
    val buffer = if event.getBuffer.startsWith("/") then event.getBuffer.substring(1) else event.getBuffer

    val args = RawCmdArg.stringToRawArgsQuoted(buffer)
    if event.isCommand && args.nonEmpty && names.contains(args.head.content) then
      val res = Await.result(executions.tabCompleteAsync(event.getSender, args.tail).asFuture, 1.seconds)
      event.setCompletions(res.asJava)

  def register(plugin: ScalaPlugin): Unit =
    Bukkit.getPluginManager.registerEvents(this, plugin)

    plugin.addDisableAction(unregister(plugin))

    for name <- names do
      val pluginCommand = plugin.getCommand(name)
      pluginCommand.setExecutor(this)

  def registerCommandMap(plugin: ScalaPlugin, fallbackPrefix: String): Unit =
    Bukkit.getServer.getCommandMap.register(fallbackPrefix, this)
    Bukkit.getPluginManager.registerEvents(this, plugin)

  private[command] def unregister(plugin: ScalaPlugin): Unit =
    for name <- names do
      val pluginCommand = plugin.getCommand(name)
      pluginCommand.setExecutor(null)

object Command:

  def apply(name: String, permission: String = "")(firstExecution: Executions, executions: Executions*)(
      using ExecutionContext
  ): Command =
    new Command(
      Seq(name),
      if (permission.isEmpty) None else Some(permission),
      AggExecutions(NonEmptyList.of(firstExecution, executions: _*)).flatten
    )

  def fromExecutionBuilder(name: String, permission: String = "")(executions: ExecutionBuilder ?=> Unit)(
      using ExecutionContext,
      ScalaPlugin
  ): Command =
    val builder = new ExecutionBuilder
    executions(using builder)
    Command(name, permission)(builder.executions.executions.head, builder.executions.executions.tail: _*)

  def apply(permission: Option[String], names: String*)(firstExecution: Executions, executions: Executions*)(
      using ExecutionContext
  ): Command =
    new Command(names, permission, AggExecutions(NonEmptyList.of(firstExecution, executions: _*)).flatten)

  def fromExecutionBuilder(permission: Option[String], names: String*)(executions: ExecutionBuilder ?=> Unit)(
      using ExecutionContext,
      ScalaPlugin
  ): Command =
    val builder = new ExecutionBuilder
    executions(using builder)
    Command(permission, names: _*)(builder.executions.executions.head, builder.executions.executions.tail: _*)

  def execution[Args, Sender](
      args: Parameter[Args] = Parameters.unit,
      sender: UserValidator[Sender] = Senders.commandSender,
      permissions: String = "",
      help: CommandSender => Option[Text] = _ => None,
      description: CommandSender => Option[Text] = _ => None
  )(
      f: (Sender, Args) => Either[String, Unit]
  )(using plugin: ScalaPlugin): Executions =
    new RunExecution[Args, Args, Sender](
      plugin,
      if (permissions.isEmpty) None else Some(permissions),
      help,
      description,
      sender,
      args,
      identity,
      f
    )

  def asyncExecution[Args, Sender](
      args: Parameter[Args] = Parameters.unit,
      sender: UserValidator[Sender] = Senders.commandSender,
      permissions: String = "",
      help: CommandSender => Option[Text] = _ => None,
      description: CommandSender => Option[Text] = _ => None
  )(
      f: ExecutionContext ?=> (Sender, Args) => FutureOrNow[Either[String, Unit]]
  )(using plugin: ScalaPlugin): Executions =
    new RunAsyncExecution[Args, Args, Sender](
      plugin,
      if (permissions.isEmpty) None else Some(permissions),
      help,
      description,
      sender,
      args,
      identity,
      implicit ec => f(using ec)
    )

  def withArg[Args](arg: Parameter[Args])(f: Parameter[Args] => NonEmptyList[Executions]): Executions =
    new AggExecutions(f(arg))

  def withArgCached[Args](arg: Parameter[Args])(f: Parameter[Args] => NonEmptyList[Executions]): Executions =
    new CachedParamExecution(arg, f)

  def subCommand(name: String, names: String*)(child: Executions, children: Executions*): Executions =
    new AggExecutions(
      NonEmptyList.of(child, children: _*).map(_.addArgFirst(Parameter.literal(NonEmptyList.of(name, names: _*))))
    )
