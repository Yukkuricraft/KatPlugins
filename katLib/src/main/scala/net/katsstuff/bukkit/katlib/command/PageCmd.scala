package net.katsstuff.bukkit.katlib.command

import java.util.UUID
import java.util.concurrent.TimeUnit

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.jdk.CollectionConverters.*

import com.google.common.cache.CacheBuilder
import net.katsstuff.bukkit.katlib.ScalaPlugin
import net.katsstuff.bukkit.katlib.command.~
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

  def command(using ExecutionContext, ScalaPlugin): Command = Command("page")(
    execution(optional(("next" | "prev") || Parameters.int.named("page")) ~ optional(Parameters.uuid)) {
      case (sender, pageToSend ~ uuid) =>
        for
          pageUuid <- uuid.orElse(focusedPages.get(sender)).toRight("No active page")
          page <- pageToSend match {
            case None               => currentPage(sender, pageUuid).toRight("No current page")
            case Some(Left("prev")) => prevPage(sender, pageUuid).toRight("No previous page")
            case Some(Left("next")) => nextPage(sender, pageUuid).toRight("No next page")
            case Some(Right(num))   => gotoPage(sender, pageUuid, num).toRight(s"No page with number $num")
            case Some(Left(_))      => Left("Invalid argument")
          }
          _ = sender.sendMessage(page)
        yield ()
    }
  )

  def currentPage(sender: CommandSender, uuid: UUID): Option[Text] = allPages(sender).get(uuid).map(_.focus)

  private def getPage(sender: CommandSender, uuid: UUID, action: Zipper[Text] => Option[Zipper[Text]]): Option[Text] =
    for
      zipper <- allPages(sender).get(uuid)
      next   <- action(zipper)
    yield
      allPages(sender).put(uuid, next)
      focusedPages.put(sender, uuid)
      next.focus

  def nextPage(sender: CommandSender, uuid: UUID): Option[Text]         = getPage(sender, uuid, _.right)
  def prevPage(sender: CommandSender, uuid: UUID): Option[Text]         = getPage(sender, uuid, _.left)
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
end PageCmd
