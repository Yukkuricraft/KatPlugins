package net.katsstuff.bukkit.katlib.brigcommands

import java.util.UUID
import java.util.concurrent.{CompletableFuture, TimeUnit}

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.jdk.CollectionConverters.*
import scala.util.boundary.Label

import com.google.common.cache.CacheBuilder
import com.mojang.brigadier.arguments.{ArgumentType, IntegerArgumentType, StringArgumentType}
import com.mojang.brigadier.context.CommandContext
import com.mojang.brigadier.exceptions.{CommandExceptionType, SimpleCommandExceptionType}
import com.mojang.brigadier.suggestion.{Suggestions, SuggestionsBuilder}
import com.mojang.brigadier.{LiteralMessage, StringReader}
import io.papermc.paper.command.brigadier.CommandSourceStack
import io.papermc.paper.command.brigadier.argument.{ArgumentTypes, CustomArgumentType}
import net.katsstuff.bukkit.katlib.ScalaPlugin
import net.katsstuff.bukkit.katlib.helper.Zipper
import net.katsstuff.bukkit.katlib.text.*
import org.bukkit.command.CommandSender

class PageCmd:

  private val allPages = new mutable.WeakHashMap[CommandSender, mutable.Map[UUID, Zipper[Text]]].withDefault { _ =>
    CacheBuilder
      .newBuilder()
      .expireAfterAccess(5, TimeUnit.MINUTES)
      .build[UUID, Zipper[Text]]()
      .asMap
      .asScala
  }

  private val focusedPages = new mutable.WeakHashMap[CommandSender, UUID]

  // noinspection UnstableApiUsage
  def executesPage(
      specifiedPagination: ArgWrapper[Option[UUID]],
      getPage: (CommandContext[Source], Label[CommandResult.Error]) ?=> (CommandSender, UUID) => Option[Text],
      errorMsg: (CommandContext[Source], Label[CommandResult.Error]) ?=> String
  )(using BuilderArgs): Unit =
    executes():
      val sender = useSender()
      val currentPagination = specifiedPagination.get match
        case Some(pagination) => pagination
        case None             => focusedPages.get(sender).getOrError("No active pagination")
      sender.sendMessage(getPage(sender, currentPagination).getOrError(errorMsg))
      1

  // noinspection UnstableApiUsage
  def command(using ExecutionContext, ScalaPlugin): BrigRootCommand =
    rootLiteral("page"):
      literals("prev", "next"): dir =>
        optArg("pagination", ArgumentTypes.uuid()): getPagination =>
          executesPage(
            getPagination,
            dir match
              case "prev" => prevPage
              case "next" => nextPage,
            dir match
              case "prev" => "No previous page"
              case "next" => "No next page"
          )

      arg("page", IntegerArgumentType.integer(1)): getPage =>
        optArg("pagination", ArgumentTypes.uuid()): getPagination =>
          executesPage(
            getPagination,
            gotoPage(_, _, getPage.get),
            s"No page with number ${getPage.get}"
          )

      optArg("pagination", ArgumentTypes.uuid()): getPagination =>
        executesPage(getPagination, currentPage, "No current page")

  def currentPage(sender: CommandSender, uuid: UUID): Option[Text] = allPages(sender).get(uuid).map(_.focus)

  private def getPage(sender: CommandSender, uuid: UUID, action: Zipper[Text] => Option[Zipper[Text]]): Option[Text] =
    for
      zipper <- allPages(sender).get(uuid)
      next   <- action(zipper)
    yield
      allPages(sender).put(uuid, next)
      focusedPages.put(sender, uuid)
      next.focus

  def nextPage(sender: CommandSender, uuid: UUID): Option[Text] = getPage(sender, uuid, _.right)

  def prevPage(sender: CommandSender, uuid: UUID): Option[Text] = getPage(sender, uuid, _.left)

  def gotoPage(sender: CommandSender, uuid: UUID, i: Int): Option[Text] = getPage(sender, uuid, _.goto(i))

  def newPages(sender: CommandSender, pages: UUID => Seq[Text]): Text =
    val uuid         = UUID.randomUUID()
    val createdPages = pages(uuid)
    val zipper       = Zipper(Nil, createdPages.head, createdPages.tail)

    val senderPages = allPages(sender)
    senderPages.put(uuid, zipper)
    focusedPages.put(sender, uuid)
    allPages.put(sender, senderPages)

    createdPages.head
