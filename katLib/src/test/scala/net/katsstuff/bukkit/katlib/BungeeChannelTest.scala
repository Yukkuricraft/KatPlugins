package net.katsstuff.bukkit.katlib

import java.io.{ByteArrayInputStream, DataInputStream}

import net.katsstuff.bukkit.katlib.testing.{BukkitSuite, RecordingPlayerMock, TestScalaPlugin}
import net.katsstuff.bukkit.katlib.text.*
import org.bukkit.plugin.messaging.ChannelNotRegisteredException

class BungeeChannelTest extends BukkitSuite:

  /** The messages sent to the proxy, as their subchannel and arguments. */
  private def sentMessages(player: RecordingPlayerMock, plugin: ScalaPlugin): Seq[Seq[String]] =
    player.pluginMessages.map { (source, channel, bytes) =>
      assertEquals(source, plugin)
      assertEquals(channel, "BungeeCord")
      val in = new DataInputStream(new ByteArrayInputStream(bytes))
      Iterator.continually(in).takeWhile(_.available() > 0).map(_.readUTF()).toSeq
    }

  private def setup(): (ScalaPlugin, BungeeChannel, RecordingPlayerMock) =
    given plugin: ScalaPlugin = createPlugin[ScalaPlugin]("Test", classOf[TestScalaPlugin])
    server.getMessenger.registerOutgoingPluginChannel(plugin, "BungeeCord")
    val player = new RecordingPlayerMock(server, "Sender")
    server.addPlayer(player)
    (plugin, new BungeeChannel, player)

  test("sending a player to a server") {
    val (plugin, channel, player) = setup()
    channel.sendPlayerToServer(player, "lobby")
    assertEquals(sentMessages(player, plugin), Seq(Seq("Connect", "lobby")))
  }

  test("sending another player to a server") {
    val (plugin, channel, player) = setup()
    channel.sendOtherPlayerToServer(player, "Other", "lobby")
    assertEquals(sentMessages(player, plugin), Seq(Seq("ConnectOther", "Other", "lobby")))
  }

  test("messaging a player on another server") {
    val (plugin, channel, player) = setup()
    channel.sendMessage(player, "Other", t"Hello")
    val sent = sentMessages(player, plugin)
    assertEquals(sent.map(_.take(2)), Seq(Seq("MessageRaw", "Other")))
    assert(sent.head(2).contains("Hello"), sent.head(2))
  }

  test("the BungeeCord channel must be registered to send anything") {
    given plugin: ScalaPlugin = createPlugin[ScalaPlugin]("Test", classOf[TestScalaPlugin])
    val channel               = new BungeeChannel
    intercept[ChannelNotRegisteredException](channel.sendPlayerToServer(server.addPlayer(), "lobby"))
  }
