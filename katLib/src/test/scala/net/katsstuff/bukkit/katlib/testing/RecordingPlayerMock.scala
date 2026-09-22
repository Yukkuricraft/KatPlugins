package net.katsstuff.bukkit.katlib.testing

import java.util.UUID
import java.util.concurrent.CopyOnWriteArrayList

import scala.jdk.CollectionConverters.*

import be.seeseemelk.mockbukkit.ServerMock
import be.seeseemelk.mockbukkit.entity.PlayerMock
import org.bukkit.plugin.Plugin

/** A player that remembers the plugin messages sent through it. */
class RecordingPlayerMock(server: ServerMock, name: String, uuid: UUID = UUID.randomUUID())
    extends PlayerMock(server, name, uuid):
  private val sent = new CopyOnWriteArrayList[(Plugin, String, Array[Byte])]()

  override def sendPluginMessage(source: Plugin, channel: String, message: Array[Byte]): Unit =
    super.sendPluginMessage(source, channel, message)
    sent.add((source, channel, message))

  def pluginMessages: Seq[(Plugin, String, Array[Byte])] = sent.asScala.toSeq
