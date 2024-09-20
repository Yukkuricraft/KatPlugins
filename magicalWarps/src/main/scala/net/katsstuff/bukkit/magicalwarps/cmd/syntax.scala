package net.katsstuff.bukkit.magicalwarps.cmd

import net.katsstuff.bukkit.katlib.text.*
import net.kyori.adventure.text.event.ClickEvent

def confirmButton(label: Text, command: String): Text =
  t"[$label]".clickEvent(ClickEvent.suggestCommand(command))

def button(label: Text, command: String): Text =
  t"[$label]".clickEvent(ClickEvent.runCommand(command)).hoverEvent(t"$command")

def bigButton(command: (Text, String)): Text =
  t"""[[${command._1}]]""".clickEvent(ClickEvent.runCommand(command._2)).hoverEvent(t"${command._2}")

def bigButton4(
    command1: (Text, String),
    command2: (Text, String),
    command3: (Text, String),
    command4: (Text, String)
): Text =
  val button1 = bigButton(command1)
  val button2 = bigButton(command2)
  val button3 = bigButton(command3)
  val button4 = bigButton(command4)

  t"""$button1 $button2 $button3 $button4"""
end bigButton4

inline val InvalidWarp = "Did not find the location for that world. Is the world loaded"
