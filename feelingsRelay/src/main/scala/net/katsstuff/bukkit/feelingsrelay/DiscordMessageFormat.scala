package net.katsstuff.bukkit.feelingsrelay

import java.util.Date

import scala.jdk.CollectionConverters.*

import github.scarsz.discordsrv.dependencies.commons.lang3.StringUtils
import github.scarsz.discordsrv.dependencies.jda.api.entities.MessageEmbed
import github.scarsz.discordsrv.objects.MessageFormat
import io.circe.*
import io.circe.syntax.*
import net.katsstuff.bukkit.katlib.ScalaPlugin

case class DiscordMessageFormat(
    enabled: Boolean = false,
    webhook: Option[DiscordMessageFormat.DiscordMessageFormatWebhook],
    content: String = "",
    embed: Option[DiscordMessageFormat.DiscordMessageFormatEmbed]
) derives CapitalizeDefaultsDecoder:
  import DiscordMessageFormat._

  def toMessageFormat(feeling: String)(implicit scalaPlugin: ScalaPlugin): MessageFormat =
    if !enabled
    then null
    else {
      def ifNotBlank(s: String)(f: String => Unit): Unit =
        if StringUtils.isNotBlank(s) then f(s)

      val messageFormat = new MessageFormat

      embed.filter(_.enabled).foreach { embed =>
        embed.color.foreach {
          case HexOrIntColor.HexColor(hexColor) =>
            val rawHex = hexColor.trim
            val hex    = if (!rawHex.startsWith("#")) "#" + rawHex else rawHex
            if hex.length == 7 then
              messageFormat.setColorRaw(
                (Integer.valueOf(hex.substring(1, 3), 16) << 16)
                  | (Integer.valueOf(hex.substring(3, 5), 16) << 8)
                  | Integer.valueOf(hex.substring(5, 7), 16)
              )
            else scalaPlugin.logger.debug("Invalid color hex: " + hex + " (in " + feeling + ".Embed.Color)")
          case HexOrIntColor.IntColor(color) => messageFormat.setColorRaw(color)
        }

        embed.author.foreach { author =>
          ifNotBlank(author.name)(messageFormat.setAuthorName)
          ifNotBlank(author.url)(messageFormat.setAuthorUrl)
          ifNotBlank(author.imageUrl)(messageFormat.setAuthorImageUrl)
        }

        ifNotBlank(embed.thumbnailUrl)(messageFormat.setThumbnailUrl)

        embed.title.foreach { title =>
          ifNotBlank(title.text)(messageFormat.setTitle)
          ifNotBlank(title.url)(messageFormat.setTitleUrl)
        }

        ifNotBlank(embed.description)(messageFormat.setDescription)

        if embed.fields.nonEmpty then
          val fields = embed.fields.flatMap { s =>
            if s.contains(";") then
              val parts = s.split(";")
              if parts.length < 2
              then None
              else
                val inline = parts.length < 3 || parts(2).toBoolean
                Some(new MessageEmbed.Field(parts(0), parts(1), inline, true))
            else
              val inline = s.toBoolean
              Some(new MessageEmbed.Field("\u200e", "\u200e", inline, true))
          }.asJava

          messageFormat.setFields(fields)
        end if

        ifNotBlank(embed.imageUrl)(messageFormat.setImageUrl)

        embed.footer.foreach { footer =>
          ifNotBlank(footer.text)(messageFormat.setFooterText)
          ifNotBlank(footer.iconUrl)(messageFormat.setFooterIconUrl)
        }

        embed.timestamp.foreach {
          case TimestampOrNow.NowBool(true)             => messageFormat.setTimestamp(new Date().toInstant)
          case TimestampOrNow.TimestampEpoch(timestamp) => messageFormat.setTimestamp(new Date(timestamp).toInstant)
          case _                                        =>
        }
      }

      webhook.filter(_.enable).foreach { webhook =>
        messageFormat.setUseWebhooks(true)
        ifNotBlank(webhook.avatarUrl)(messageFormat.setWebhookAvatarUrl)
        ifNotBlank(webhook.name)(messageFormat.setWebhookName)
      }

      ifNotBlank(content)(messageFormat.setContent)

      if messageFormat.isAnyContent
      then messageFormat
      else null
    }
  end toMessageFormat

object DiscordMessageFormat:
  enum HexOrIntColor:
    case HexColor(s: String)
    case IntColor(i: Int)

  object HexOrIntColor:
    given Decoder[HexOrIntColor] =
      (c: HCursor) => c.as[String].map(HexColor.apply).orElse(c.as[Int].map(IntColor.apply))

  enum TimestampOrNow:
    case TimestampEpoch(epoch: Long)
    case NowBool(includeTimestamp: Boolean)

  object TimestampOrNow:
    given Decoder[TimestampOrNow] =
      (c: HCursor) => c.as[Long].map(TimestampEpoch.apply).orElse(c.as[Boolean].map(NowBool.apply))

  case class DiscordMessageFormatWebhook(enable: Boolean = false, avatarUrl: String = "", name: String = "")
      derives CapitalizeDefaultsDecoder

  case class DiscordMessageFormatEmbed(
      enabled: Boolean = true,
      color: Option[HexOrIntColor],
      author: Option[DiscordMessageFormatEmbedAuthor],
      thumbnailUrl: String = "",
      title: Option[DiscordMessageFormatEmbedTitle],
      description: String = "",
      fields: Seq[String] = Seq.empty,
      imageUrl: String = "",
      footer: Option[DiscordMessageFormatEmbedFooter],
      timestamp: Option[TimestampOrNow]
  ) derives CapitalizeDefaultsDecoder

  case class DiscordMessageFormatEmbedAuthor(imageUrl: String = "", name: String = "", url: String = "")
      derives CapitalizeDefaultsDecoder

  case class DiscordMessageFormatEmbedTitle(text: String = "", url: String = "") derives CapitalizeDefaultsDecoder

  case class DiscordMessageFormatEmbedFooter(text: String = "", iconUrl: String = "") derives CapitalizeDefaultsDecoder
