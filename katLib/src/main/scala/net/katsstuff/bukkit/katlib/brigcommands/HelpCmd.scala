package net.katsstuff.bukkit.katlib.brigcommands

import java.util.concurrent.TimeUnit

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

import cats.data.StateT
import cats.instances.list.*
import cats.syntax.all.*
import com.google.common.cache.CacheBuilder
import com.mojang.brigadier.arguments.StringArgumentType
import net.katsstuff.bukkit.katlib.ScalaPlugin
import net.katsstuff.bukkit.katlib.service.PaginationService
import net.katsstuff.bukkit.katlib.text.*
import net.kyori.adventure.text.Component
import org.bukkit.Bukkit
import org.bukkit.command.CommandSender

class HelpCmd(plugin: ScalaPlugin):

  private val registeredCommands: mutable.HashSet[CommandInfo] = mutable.HashSet.empty
  private val commandTreeCache = CacheBuilder
    .newBuilder()
    .expireAfterWrite(2L, TimeUnit.MINUTES)
    .build[(CommandSender, CommandInfo), Seq[Text]]()
    .asMap()
    .asScala
  private val infoMatchesCache = CacheBuilder
    .newBuilder()
    .expireAfterWrite(10L, TimeUnit.MINUTES)
    .build[(String, CommandInfo), Boolean]()
    .asMap()
    .asScala

  val node: BrigRootCommand = rootLiteral("help"):
    optArg("command", StringArgumentType.greedyString()): getArg =>
      executes(
        help = _ => t"This command. Type in a partial command, or don't",
        description = _ => t"The help command where you can read more about other commands"
      ):
        val source = useSource()
        val sender = source.getSender
        val str    = getArg.get.getOrElse("")

        val pages = Bukkit.getServicesManager.load(classOf[PaginationService])
        val args  = str.split(" ").toSeq // TODO: Split this better, taking into account quotes and such

        val commandsToProcess = args.headOption
          .filter(_.nonEmpty)
          .fold(registeredCommands.toSeq) { head =>
            registeredCommands.toSeq.filter(_.printUsage.startsWith(head.toLowerCase))
          }
          .flatMap(_.filterRequires(source))

        if commandsToProcess.isEmpty then cmdError(s"No commands match ${args.head}")
        else if args.lengthIs == 1 then
          val info    = commandsToProcess.reduce(CommandInfo.merge).flatten
          val content = getCommandTree(sender, info)
          pages
            .copyObj(
              title = Some(t"${plugin.getName} Help"),
              content = content
            )
            .sendTo(sender)
          1
        else
          val matchingCommandInfos = commandsToProcess.filter(commandSyntaxMatches(args.tail))

          if matchingCommandInfos.isEmpty then cmdError(s"No commands matched ${args.mkString(" ")}")
          else
            val commandInfoMatchingSyntax = matchingCommandInfos.reduce(CommandInfo.merge).flatten
            val content                   = buildCommandTree(sender, commandInfoMatchingSyntax, detail = true)
            pages
              .copyObj(
                title = Some(t"${plugin.getName} Help"),
                content = content
              )
              .sendTo(sender)
            1
  end node

  private def commandSyntaxMatches(filters: Seq[String])(
      info: CommandInfo
  ): Boolean =
    infoMatchesCache.getOrElseUpdate(
      (filters.mkString(" "), info),
      matchParts(info)
        .run(filters.toList)
        .collectFirst { case (Nil, true) =>
          ()
        }
        .isDefined
    )

  private def matchParts(usage: CommandInfo): StateT[List, List[String], Boolean] =
    StateT.get[List, List[String]].flatMap {
      case Nil => StateT.pure(true)
      case x :: xs =>
        usage match
          case CommandInfo.Required(_, _)  => StateT.set(xs).as(x == "*")
          case CommandInfo.Const(s, _)     => StateT.set(xs).as(x == "*" || x.toLowerCase == s.toLowerCase)
          case CommandInfo.Seq(head, tail) => (head :: tail.toList).map(matchParts).reduce(_.map2(_)(_ && _))
          case CommandInfo.Choice(choices, _) =>
            StateT[List, List[String], Boolean](s => choices.map(matchParts).flatMap(_.run(s)))
          case CommandInfo.Info(_, _, _) => StateT.set(x :: xs).as(true)
    }

  private def getCommandTree(sender: CommandSender, info: CommandInfo): Seq[Text] =
    commandTreeCache.getOrElseUpdate(
      (sender, info),
      buildCommandTree(sender, info, detail = false)
    )

  private def buildCommandTree(
      sender: CommandSender,
      info: CommandInfo,
      detail: Boolean
  ): Seq[Text] =
    def treeRec(previous: Text, info: CommandInfo): Seq[Text] =
      info match
        case CommandInfo.Required(s, _) => Seq(t"$previous<$s>")
        case CommandInfo.Const(s, _)    => Seq(t"$previous$s")
        case CommandInfo.Seq(head, tail) =>
          (head :: tail).foldLeft(Seq(previous))((accs, info) => accs.flatMap(treeRec(_, info)))
        case CommandInfo.Choice(choices, _) =>
          val seq = choices.toIndexedSeq
          Seq(
            Seq(previous),
            seq.init.flatMap(i => treeRec(t"├─ $Green$Underlined", i)),
            seq.lastOption.toSeq.flatMap(i => treeRec(t"└─ $Green$Underlined", i))
          ).flatten

        case CommandInfo.Info(help, description, redirectTo) =>
          lazy val commandHelp   = Some(help(sender)).filter(t => t != Text.Empty)
          val commandDescription = Some(description(sender)).filter(t => t != Text.Empty)

          val withHover      = commandDescription.fold(previous)(desc => previous.hoverEvent(desc))
          val redirectToInfo = if redirectTo.isEmpty then "" else s" -> $redirectTo"

          Seq(
            if detail then
              commandHelp
                .orElse(commandDescription)
                .fold(t"$withHover$redirectToInfo")(desc => t"$withHover - $desc$redirectToInfo")
            else commandDescription.fold(t"$withHover$redirectToInfo")(desc => t"$withHover - $desc$redirectToInfo")
          )
    end treeRec

    treeRec(t"$Green$Underlined/", info)

  def registerCommand(command: CommandInfo): Unit = registeredCommands.add(command)
