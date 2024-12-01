package net.katsstuff.bukkit.katlib.brigcommands

import com.mojang.brigadier.arguments.{IntegerArgumentType, StringArgumentType}
import net.katsstuff.bukkit.katlib.text.*

object Testing:

  rootLiteral("foo"):
    literal("bar"):
      optArg("arg1", IntegerArgumentType.integer(), requirement = permission("foo.bar")): arg1Get =>
        arg("arg2", StringArgumentType.string()): arg2Get =>
          executes(description = _ => t"Foo bar"):
            val a1 ~ a2 = (arg1Get ~ arg2Get).get
            val r       = a1.map(_ + a2.toInt)

            val ping = usePlayerSender().getPing

            r.getOrElse(0) + ping

        executes(description = _ => t"Bar foo"):
          val a1 = arg1Get.get
          a1.map(v => v: Int).getOrElse(0)
