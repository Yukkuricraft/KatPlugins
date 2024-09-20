package net.katsstuff.bukkit.homesweethome.cmd

import net.katsstuff.bukkit.katlib.text.*
import net.kyori.adventure.text.event.ClickEvent

def confirmButton(button: Text, text: String): Text =
  t"[$button]".clickEvent(ClickEvent.suggestCommand(text))

def button(button: Text, text: String): Text =
  t"[$button]".clickEvent(ClickEvent.runCommand(text)).hoverEvent(t"$text")

inline val HomeNotFound = "No home with that name found"
