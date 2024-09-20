package net.katsstuff.bukkit.katlib.command

import java.util.concurrent.TimeUnit

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

import cats.data.{NonEmptyList, StateT}
import cats.instances.list.*
import cats.syntax.all.*
import com.google.common.cache.CacheBuilder
import net.katsstuff.bukkit.katlib.ScalaPlugin
import net.katsstuff.bukkit.katlib.service.PaginationService
import net.katsstuff.bukkit.katlib.text.*
import org.bukkit.Bukkit
import org.bukkit.command.CommandSender

class HelpCmd(plugin: ScalaPlugin):

  private val registeredCommands: mutable.HashSet[Command] = mutable.HashSet.empty
  private val commandTreeCache = CacheBuilder
    .newBuilder()
    .expireAfterWrite(2L, TimeUnit.MINUTES)
    .build[(CommandSender, Command), Seq[Text]]()
    .asMap()
    .asScala
  private val usageMatchesCache = CacheBuilder
    .newBuilder()
    .expireAfterWrite(10L, TimeUnit.MINUTES)
    .build[(String, Usage), Boolean]()
    .asMap()
    .asScala

  val helpExecution: Executions =
    given ScalaPlugin = plugin
    execution(Parameters.remainingAsString) { case (sender, str) =>
      val pages = Bukkit.getServicesManager.load(classOf[PaginationService])
      val args = str.split(" ").toSeq //TODO: Split this better, taking into account quotes and such

      val commandsToProcess = args.headOption
        .filter(_.nonEmpty)
        .fold(registeredCommands.toSeq) { head =>
          registeredCommands.toSeq.filter(_.names.contains(head.toLowerCase))
        }
        .filter(_.permission.fold(true)(sender.hasPermission))

      if commandsToProcess.isEmpty then Left(s"No commands match ${args.head}")
      else if args.lengthIs < 2 then
        val content = commandsToProcess.sortBy(_.names.head).flatMap(buildCommandTree(sender, _))
        pages
          .copyObj(
            title = Some(t"${plugin.getName} Help"),
            content = content
          )
          .sendTo(sender)
        Right(())
      else
        val allExecutions      = commandsToProcess.head.executions.runExecutions
        val matchingExecutions = allExecutions.filter(executionUsageMatches(sender, args.tail))

        if matchingExecutions.isEmpty then Left(s"No executions matched ${args.mkString(" ")}")
        else
          val content = matchingExecutions.map(buildExecutionEntry(sender, args.head, _, detail = true))
          pages
            .copyObj(
              title = Some(t"${plugin.getName} Help"),
              content = content
            )
            .sendTo(sender)
          Right(())
    }

  private def executionUsageMatches(sender: CommandSender, filters: Seq[String])(
      ex: BaseRunExecution[?, ?, ?]
  ): Boolean =
    if ex.permissions.forall(sender.hasPermission) then
      val usage = ex.allArgs.usage(sender)
      usageMatchesCache.getOrElseUpdate(
        (filters.mkString(" "), usage),
        matchParts(usage)
          .run(filters.toList)
          .collectFirst { case (Nil, true) =>
            ()
          }
          .isDefined
      )
    else false

  private def matchParts(usage: Usage): StateT[List, List[String], Boolean] =
    StateT.get[List, List[String]].flatMap {
      case Nil => StateT.pure(true)
      case x :: xs =>
        usage match
          case Usage.Required(_)     => StateT.set(xs).as(x == "*")
          case Usage.Const(s)        => StateT.set(xs).as(x == "*" || x.toLowerCase == s.toLowerCase)
          case Usage.Seq(head, tail) => (head :: tail.toList).map(matchParts).reduce(_.map2(_)(_ && _))
          case Usage.Choice(choices, _) =>
            StateT[List, List[String], Boolean](s => choices.map(matchParts).flatMap(_.run(s)))
    }

  private def buildCommandTree(sender: CommandSender, command: Command): Seq[Text] =
    commandTreeCache.getOrElseUpdate(
      (sender, command),
      command.executions.runExecutions.toList.map(buildExecutionEntry(sender, command.names.head, _, detail = false))
    )

  private def buildExecutionEntry(
      sender: CommandSender,
      commandName: String,
      executions: BaseRunExecution[?, ?, ?],
      detail: Boolean
  ): Text =
    val usage    = executions.allArgs.usage(sender)
    val helpBase = t"$Green$Underlined/$commandName ${usage.printUsage}"

    val commandHelp        = executions.help(sender)
    val commandDescription = executions.description(sender)
    
    val withHover = commandDescription.fold(helpBase)(desc => helpBase.hoverEvent(t"$desc"))

    if detail then commandHelp.orElse(commandDescription).fold(withHover)(desc => t"$withHover - $desc")
    else commandDescription.fold(withHover)(desc => t"$withHover - $desc")

  def registerCommand(command: Command): Unit = registeredCommands.add(command)
