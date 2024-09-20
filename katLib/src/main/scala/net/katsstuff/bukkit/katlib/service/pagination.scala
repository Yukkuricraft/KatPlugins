package net.katsstuff.bukkit.katlib.service

import java.util.UUID
import net.katsstuff.bukkit.katlib.ScalaPlugin
import net.katsstuff.bukkit.katlib.text.*
import net.kyori.adventure.text.{Component, JoinConfiguration}
import net.kyori.adventure.text.event.ClickEvent
import net.kyori.adventure.text.serializer.plain.PlainTextComponentSerializer
import org.bukkit.command.CommandSender

trait PaginationService:

  def title: Option[Text]

  def header: Option[Text]

  def footer: Option[Text]

  def padding: Text

  def linesPerPage: Int

  def content: Seq[Text]

  def copyObj(
      title: Option[Text] = title,
      header: Option[Text] = header,
      footer: Option[Text] = footer,
      padding: Text = padding,
      linesPerPage: Int = linesPerPage,
      content: Seq[Text] = content
  ): PaginationService

  def sendTo(source: CommandSender): Unit
end PaginationService

case class SimplePagination(
    title: Option[Text] = None,
    header: Option[Text] = None,
    footer: Option[Text] = None,
    padding: Text = t"=",
    linesPerPage: Int = 19,
    content: Seq[Text] = Seq()
)(using sc: ScalaPlugin)
    extends PaginationService:

  override def sendTo(source: CommandSender): Unit = if (content.nonEmpty) {
    val paddingFiller = Text(Seq.fill(10)(padding): _*)

    def createNextButton(uuid: UUID, pageNum: Int, pageEnd: Int): Text =
      if pageNum == pageEnd - 1 then Text.Empty
      else
        val basic     = t">>"
        val withHover = basic.hoverEvent(t"Next page")
        withHover.clickEvent(ClickEvent.runCommand(s"/${sc.getName}:page next ${uuid.toString}"))
    end createNextButton

    def createPrevButton(uuid: UUID, pageNum: Int): Text =
      if pageNum == 0 then Text.Empty
      else
        val basic     = t"<<"
        val withHover = basic.hoverEvent(t"Previous page")
        withHover.clickEvent(ClickEvent.runCommand(s"/${sc.getName}:page prev ${uuid.toString}"))
    end createPrevButton

    val rawPages = content.grouped(linesPerPage - 2).toSeq.zipWithIndex
    val createPages = (uuid: UUID) => {
      val usedTitle = title.getOrElse(Text.Empty)

      val plainTextSerializer = PlainTextComponentSerializer.plainText()

      val titleLength = plainTextSerializer.serialize(usedTitle).length
      val pageEnd     = rawPages.size
      rawPages.map { case (page, pageNum) =>
        val prevButton = createPrevButton(uuid, pageNum)
        val nextButton = createNextButton(uuid, pageNum, pageEnd)

        val bottomText   = t"$Yellow$prevButton Page ${pageNum + 1} of $pageEnd $nextButton"
        val bottomLength = plainTextSerializer.serialize(bottomText).length

        val extraPaddingBottom = Seq.fill((titleLength - bottomLength) / 2)(padding)
        val extraPaddingTop    = Seq.fill((bottomLength - titleLength) / 2)(padding)

        val top = Text((paddingFiller +: extraPaddingTop) ++ (usedTitle +: extraPaddingTop :+ paddingFiller): _*)
        val bottom =
          Text((paddingFiller +: extraPaddingBottom) ++ (bottomText +: extraPaddingBottom :+ paddingFiller): _*)

        val texts =
          (Seq(top, header.getOrElse(Text.Empty)) ++ page ++ Seq(footer.getOrElse(Text.Empty), bottom))
            .filter(_ != Text.Empty)
          
        Component.join(JoinConfiguration.newlines(), texts*).compact()
      }
    }

    val firstPage = sc.pageCmd.newPages(source, createPages)
    source.sendMessage(firstPage)
  }

  override def copyObj(
      title: Option[Text] = title,
      header: Option[Text] = header,
      footer: Option[Text] = footer,
      padding: Text = padding,
      linesPerPage: Int = linesPerPage,
      content: Seq[Text] = content
  ): PaginationService = SimplePagination(title, header, footer, padding, linesPerPage, content)
end SimplePagination
