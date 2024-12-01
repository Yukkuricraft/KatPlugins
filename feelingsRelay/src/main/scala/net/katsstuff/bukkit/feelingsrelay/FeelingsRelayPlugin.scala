package net.katsstuff.bukkit.feelingsrelay

import java.awt.Color
import java.io.IOException
import java.nio.file.Files
import java.util.Date
import java.util.function.BiFunction
import java.util.logging.Logger

import scala.compiletime.uninitialized
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try}

import com.zachduda.chatfeelings.api.FeelingRecieveEvent
import github.scarsz.discordsrv.DiscordSRV
import github.scarsz.discordsrv.dependencies.commons.lang3.StringUtils
import github.scarsz.discordsrv.objects.MessageFormat
import github.scarsz.discordsrv.util.{DiscordUtil, MessageUtil, TimeUtil, WebhookUtil}
import io.circe.yaml.parser
import io.papermc.paper.plugin.lifecycle.event.types.LifecycleEvents
import net.katsstuff.bukkit.katlib.ScalaPlugin
import net.katsstuff.bukkit.katlib.command.Command
import net.katsstuff.bukkit.katlib.text.*
import org.bukkit.command.CommandSender
import org.bukkit.entity.Player
import org.bukkit.event.player.PlayerInteractEntityEvent
import org.bukkit.event.{EventHandler, Listener}

class FeelingsRelayPlugin extends ScalaPlugin with Listener:

  implicit val plugin: FeelingsRelayPlugin                   = this
  private var relayConfig: Map[String, DiscordMessageFormat] = uninitialized

  private def feelingsRelayCommand = Command("feelingsrelay", "feelingsrelay.admin")(
    Command.subCommand("reload")(
      Command.execution() { (sender: CommandSender, _) =>
        loadConfig().toTry match {
          case Success(newConfig) =>
            relayConfig = newConfig
            sender.sendMessage(t"${Green}Config load success")
            Right(())

          case Failure(e) =>
            e.printStackTrace()
            Left(e.getMessage)
        }

      }
    ),
    Command.execution() { (sender: CommandSender, _) =>
      sender.sendMessage(t"${Yellow}FeelingsRelay v1.0")
      Right(())
    }
  )

  // noinspection UnstableApiUsage
  override def onEnable(): Unit =
    relayConfig = loadConfig().toTry.get
    server.getPluginManager.registerEvents(this, this)
    getLifecycleManager.registerEventHandler(
      LifecycleEvents.COMMANDS.newHandler(e => feelingsRelayCommand.registerBrigadier(e.registrar, this))
    )

  override def onDisable(): Unit =
    PlayerInteractEntityEvent.getHandlerList.unregister(this: Listener)
    relayConfig = null

  def loadConfig(): Either[Throwable, Map[String, DiscordMessageFormat]] =
    val path = dataFolder.toPath.resolve("config.yml")
    logger.info("Loading Config")
    Try {
      Files.createDirectories(path.getParent)
      if Files.notExists(path) then saveResource("config.yml", false)
      Files.readAllLines(path).asScala.mkString("\n")
    }.toEither
      .flatMap(parser.parse)
      .flatMap { json =>
        val cursor = json.hcursor
        cursor.get[Int]("version").flatMap {
          case 1 => cursor.get[Map[String, DiscordMessageFormat]]("feelings")
          case _ => Left(new Exception("Unsupported version"))
        }
      }

  def getMessageFromFeeling(feeling: String): MessageFormat =
    relayConfig.get(feeling).map(_.toMessageFormat(feeling)).orNull

  // Adapted from onLeave in DiscordSRV
  @EventHandler(ignoreCancelled = true)
  def onInteract(event: FeelingRecieveEvent): Unit =
    val sender     = event.getSender
    val receiver   = event.getPlayer
    val feeling    = event.getFeeling
    val discordSRV = DiscordSRV.getPlugin

    val messageFormat = getMessageFromFeeling(feeling.capitalize)
    if messageFormat == null then
      logger.debug("Not sending feelings message, message format is null")
      return

    val textChannel = discordSRV.getMainTextChannel
    if textChannel == null then
      logger.debug("Not sending feelings message, text channel is null")
      return

    def stripIfNotBlank(s: String): String = if StringUtils.isNotBlank(s) then MessageUtil.strip(s) else ""
    def playerDisplayName(p: Player): String =
      net.kyori.adventure.text.serializer.plain.PlainTextComponentSerializer.plainText().serialize(p.displayName)

    val senderDisplayName = sender match
      case p: Player => stripIfNotBlank(playerDisplayName(p))
      case _         => stripIfNotBlank(sender.getName)
    val senderName = stripIfNotBlank(sender.getName)

    val receiverDisplayName = stripIfNotBlank(playerDisplayName(receiver))
    val receiverName        = stripIfNotBlank(receiver.getName)

    val botAvatarUrl = DiscordUtil.getJda.getSelfUser.getEffectiveAvatarUrl
    val senderAvatarUrl = sender match
      case p: Player => DiscordSRV.getAvatarUrl(p)
      case _         => botAvatarUrl
    val receiverAvatarUrl: String = DiscordSRV.getAvatarUrl(receiver)

    val botName =
      if discordSRV.getMainGuild != null
      then discordSRV.getMainGuild.getSelfMember.getEffectiveName
      else DiscordUtil.getJda.getSelfUser.getName

    val translator: BiFunction[String, java.lang.Boolean, String] =
      (content: String, needsEscape: java.lang.Boolean) => {
        if content == null
        then null
        else
          def escapeIfNeeded(s: String): String = if (needsEscape) DiscordUtil.escapeMarkdown(s) else s

          val percentReplacedContent = content
            .replaceAll("%time%|%date%", TimeUtil.timeStamp())
            .replace("%senderusername%", MessageUtil.strip(escapeIfNeeded(senderName)))
            .replace("%receiverusername%", MessageUtil.strip(escapeIfNeeded(receiverName)))
            .replace("%senderdisplayname%", escapeIfNeeded(senderDisplayName))
            .replace("%receiverdisplayname%", escapeIfNeeded(receiverDisplayName))
            .replace("%senderusernamenoescapes%", senderName)
            .replace("%receiverusernamenoescapes%", receiverName)
            .replace("%senderdisplaynamenoescapes%", senderDisplayName)
            .replace("%receiverdisplaynamenoescapes%", receiverDisplayName)
            .replace("%senderembedavatarurl%", senderAvatarUrl)
            .replace("%receiverembedavatarurl%", receiverAvatarUrl)
            .replace("%botavatarurl%", botAvatarUrl)
            .replace("%botname%", botName)

          DiscordUtil.translateEmotes(percentReplacedContent, textChannel.getGuild)
      }

    val discordMessage = DiscordSRV.translateMessage(messageFormat, translator)

    if discordMessage == null
    then return

    val webhookName      = translator.apply(messageFormat.getWebhookName, false)
    val webhookAvatarUrl = translator.apply(messageFormat.getWebhookAvatarUrl, false)

    if messageFormat.isUseWebhooks then
      WebhookUtil.deliverMessage(
        textChannel,
        webhookName,
        webhookAvatarUrl,
        discordMessage.getContentRaw,
        discordMessage.getEmbeds.stream.findFirst.orElse(null)
      )
    else DiscordUtil.queueMessage(textChannel, discordMessage)
  end onInteract
