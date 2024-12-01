package net.katsstuff.bukkit.katlib.command

case class RawCmdArg(start: Int, end: Int, content: String):

  def isEmpty: Boolean = content.isEmpty

object RawCmdArg:

  // https://stackoverflow.com/questions/249791/regex-for-quoted-string-with-escaping-quotes
  private val quotedRegex = """(?:"((?:[^"\\]|\\.)+)")|((?:\S)+)""".r

  def stringToRawArgsQuoted(arguments: String): List[RawCmdArg] =
    if arguments.isEmpty then List(RawCmdArg(0, 0, ""))
    else
      val xs = quotedRegex
        .findAllMatchIn(arguments)
        .map { m =>
          val quoted = m.group(1) != null
          val group  = if (quoted) 1 else 2
          RawCmdArg(m.start(group), m.end(group), m.group(group))
        }
        .toList

      val lastPos = arguments.length - 1
      if arguments.endsWith(" ") then xs :+ RawCmdArg(lastPos, lastPos, "")
      else xs
