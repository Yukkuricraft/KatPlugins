package net.katsstuff.bukkit.katlib

import net.katsstuff.bukkit.katlib.text.*
import net.kyori.adventure.text.serializer.json.JSONComponentSerializer
import org.bukkit.entity.Player
import org.bukkit.plugin.messaging.PluginMessageListener

import java.io.*
import java.util.concurrent.ConcurrentLinkedQueue
import scala.collection.{concurrent, mutable}
import scala.concurrent.{Future, Promise}

class BungeeChannel(using plugin: ScalaPlugin) extends PluginMessageListener:

  private val promises: mutable.Map[String, ConcurrentLinkedQueue[Promise[Array[Byte]]]] = concurrent.TrieMap()

  private def sendBungeeMessageRaw(
      player: Player,
      subchannel: String,
      writeArgs: DataOutput => Unit,
      sizeHint: Option[Int] = None
  ): Unit =
    player.sendPluginMessage(
      plugin,
      "BungeeCord",
      makeBytes(
        o => { o.writeUTF(subchannel); writeArgs(o) },
        sizeHint = Some(sizeHint.fold(subchannel.length)(subchannel.length + _))
      )
    )

  private def sendBungeeMessage(player: Player, subchannel: String, args: String*): Unit =
    sendBungeeMessageRaw(player, subchannel, o => args.foreach(o.writeUTF), sizeHint = Some(args.map(_.length).sum))

  private def sendBungeeMessageWithResponse(player: Player, subchannel: String, args: String*): Future[Array[Byte]] =
    val f = expectResponse(subchannel)
    sendBungeeMessage(player, subchannel, args*)
    f

  private def makeBytes(f: DataOutput => Unit, sizeHint: Option[Int] = None): Array[Byte] =
    val bos = new ByteArrayOutputStream(sizeHint.getOrElse(32))
    val os  = new DataOutputStream(bos)

    f(os)

    bos.toByteArray

  private def expectResponse(subchannel: String): Future[Array[Byte]] =
    val existing = promises.getOrElseUpdate(subchannel, new ConcurrentLinkedQueue[Promise[Array[Byte]]]())
    val promise  = Promise[Array[Byte]]
    existing.offer(promise)
    promise.future

  def sendPlayerToServer(player: Player, server: String): Unit =
    sendBungeeMessage(player, "Connect", server)

  def sendOtherPlayerToServer(sender: Player, toSend: String, server: String): Unit =
    sendBungeeMessage(sender, "ConnectOther", toSend, server)

  def sendMessage(sender: Player, playerName: String, message: Text): Unit =
    val jsonMessage = JSONComponentSerializer.json().serialize(message)
    sendBungeeMessage(sender, "MessageRaw", playerName, jsonMessage)

  override def onPluginMessageReceived(channel: String, player: Player, message: Array[Byte]): Unit =
    if channel == "BungeeCord" then
      val is         = new DataInputStream(new ByteArrayInputStream(message))
      val subchannel = is.readUTF()

      promises.get(subchannel).foreach { q =>
        val p = q.poll()
        if p != null then p.success(is.readAllBytes())
      }
