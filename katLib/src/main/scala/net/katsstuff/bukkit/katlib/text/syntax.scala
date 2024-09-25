package net.katsstuff.bukkit.katlib.text

import net.kyori.adventure.text as advanture

type Text = advanture.Component
object Text {
  val Empty: Text = advanture.Component.empty()

  type ApplyApplicable = advanture.ComponentBuilderApplicable | String | Byte | Short | Int | Long | Float | Double
  def apply(args: ApplyApplicable*): Text =
    val usedArgs = args.filter(_ != "")
    if usedArgs.isEmpty then Empty
    else
      try
        advanture.LinearComponents.linear(
          usedArgs.map {
            case v: String                                   => advanture.Component.text(v)
            case v: Byte                                     => advanture.Component.text(v)
            case v: Short                                    => advanture.Component.text(v)
            case v: Int                                      => advanture.Component.text(v)
            case v: Long                                     => advanture.Component.text(v)
            case v: Float                                    => advanture.Component.text(v)
            case v: Double                                   => advanture.Component.text(v)
            case other: advanture.ComponentBuilderApplicable => other
          }*
        )
      catch
        case e: IllegalStateException =>
          throw new Exception(s"Could not create text. Args: ${usedArgs.map(v => s"\"$v\"")}", e)
}

object TextStyle {
  val Bold          = advanture.format.TextDecoration.BOLD
  val Underlined    = advanture.format.TextDecoration.UNDERLINED
  val Italic        = advanture.format.TextDecoration.ITALIC
  val StrikeThrough = advanture.format.TextDecoration.STRIKETHROUGH
  val Obfuscated    = advanture.format.TextDecoration.OBFUSCATED
}
export TextStyle.*

type TextColor = advanture.format.TextColor | TextColor.NoColor.type | TextColor.Reset.type
object TextColor {
  object NoColor
  val Black: advanture.format.NamedTextColor       = advanture.format.NamedTextColor.BLACK
  val DarkBlue: advanture.format.NamedTextColor    = advanture.format.NamedTextColor.DARK_BLUE
  val DarkGreen: advanture.format.NamedTextColor   = advanture.format.NamedTextColor.DARK_GREEN
  val DarkAqua: advanture.format.NamedTextColor    = advanture.format.NamedTextColor.DARK_AQUA
  val DarkRed: advanture.format.NamedTextColor     = advanture.format.NamedTextColor.DARK_RED
  val DarkPurple: advanture.format.NamedTextColor  = advanture.format.NamedTextColor.DARK_PURPLE
  val Gold: advanture.format.NamedTextColor        = advanture.format.NamedTextColor.GOLD
  val Gray: advanture.format.NamedTextColor        = advanture.format.NamedTextColor.GRAY
  val DarkGray: advanture.format.NamedTextColor    = advanture.format.NamedTextColor.DARK_GRAY
  val Blue: advanture.format.NamedTextColor        = advanture.format.NamedTextColor.BLUE
  val Green: advanture.format.NamedTextColor       = advanture.format.NamedTextColor.GREEN
  val Aqua: advanture.format.NamedTextColor        = advanture.format.NamedTextColor.AQUA
  val Red: advanture.format.NamedTextColor         = advanture.format.NamedTextColor.RED
  val LightPurple: advanture.format.NamedTextColor = advanture.format.NamedTextColor.LIGHT_PURPLE
  val Yellow: advanture.format.NamedTextColor      = advanture.format.NamedTextColor.YELLOW
  val White: advanture.format.NamedTextColor       = advanture.format.NamedTextColor.WHITE
  object Reset

  object HexColor {
    def apply(value: Int): advanture.format.TextColor = advanture.format.TextColor.color(value)

    def apply(value: String): advanture.format.TextColor = Option(advanture.format.TextColor.fromHexString(value)).get
  }
}
export TextColor.*

extension (sc: StringContext)
  def t(args: Text.ApplyApplicable*): advanture.Component =
    StringContext.checkLengths(args, sc.parts)
    Text(sc.parts.view.zipAll(args, "", "").flatMap(t => Seq(t._1, t._2)).toSeq*)
