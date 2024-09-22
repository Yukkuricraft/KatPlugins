package net.katsstuff.bukkit.magicalwarps.cmd

import java.text.NumberFormat
import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}
import cats.data.NonEmptyList
import cats.syntax.all.*
import net.katsstuff.bukkit.katlib.{GlobalPlayer, ScalaPlugin}
import net.katsstuff.bukkit.katlib.command.*
import net.katsstuff.bukkit.katlib.service.PaginationService
import net.katsstuff.bukkit.katlib.text.*
import net.katsstuff.bukkit.katlib.util.{FutureOrNow, Teleporter}
import net.katsstuff.bukkit.magicalwarps.lib.LibPerm
import net.katsstuff.bukkit.magicalwarps.warp.Warp
import net.katsstuff.bukkit.magicalwarps.warp.storage.WarpStorage
import net.katsstuff.bukkit.magicalwarps.{WarpsConfig, WarpsPlugin}
import net.kyori.adventure.text.minimessage.MiniMessage
import net.kyori.adventure.text.serializer.legacy.LegacyComponentSerializer
import org.bukkit.command.CommandSender
import org.bukkit.entity.Player
import org.bukkit.{Bukkit, OfflinePlayer}

object Commands {

  sealed trait UserOrGroup
  object UserOrGroup {
    case class User(user: OfflinePlayer) extends UserOrGroup
    case class Group(group: String)      extends UserOrGroup
  }

  def warpParameter(using warpStorage: WarpStorage): Parameter[Warp] =
    single(
      new Parameter.ChoicesManySyncParameter[Warp](
        "warp",
        choices = warpStorage.allAccessibleWarps
      )
    )

  def warpGroupParameter(using warpStorage: WarpStorage): Parameter[WarpGroup] =
    (Parameter.choicesSingleMap("group", warpStorage.groups.map(g => g -> WarpGroup(g)).toMap) |
      Parameters.string.map(WarpGroup.apply)).named("group")

  val userOrGroupParameter: Parameter[UserOrGroup] =
    ("player" ~> single(Parameters.offlinePlayers).map[UserOrGroup](UserOrGroup.User.apply)) |
      ("group" ~> Parameters.string.named("group")).map[UserOrGroup](UserOrGroup.Group.apply)

  def warpListExecution(using warpStorage: WarpStorage)(using ScalaPlugin): Executions =
    AggExecutions(
      NonEmptyList.of(
        execution(
          sender = Senders.commandSender,
          permissions = LibPerm.List,
          description = _ => Some(t"Show all warp categories")
        ) { case (sender, _) =>
          if warpStorage.groups.nonEmpty then
            val pagination = Bukkit.getServicesManager.load(classOf[PaginationService])
            val groups     = warpStorage.groups.sorted.grouped(4).toSeq

            inline def buttonCommand(group: String): String      = s"/warp list $group"
            inline def makeButton(group: String): (Text, String) = t"${group.capitalize}" -> buttonCommand(group)

            val lines = groups.foldLeft(Seq.empty[Text]) {
              case (acc, Seq(group1, group2, group3, group4)) =>
                val buttons = bigButton4(
                  makeButton(group1),
                  makeButton(group2),
                  makeButton(group3),
                  makeButton(group4)
                )
                acc :+ buttons
              case (acc, smaller) =>
                val buttons = smaller.map(group => bigButton(makeButton(group)))
                val foldedButtons = buttons.foldLeft(Text.Empty) { case (innerAcc, btn) =>
                  t"$innerAcc $btn"
                }
                acc :+ foldedButtons
            }

            pagination.copyObj(title = Some(t"${Yellow}Groups"), content = lines).sendTo(sender)

            Right(())
          else
            sender.sendMessage(t"${Yellow}No warps created yet")
            Right(())
        },
        execution(
          warpGroupParameter,
          Senders.commandSender,
          permissions = LibPerm.List,
          description = _ => Some(t"Show all the warps in a certain category")
        ) { case (sender, WarpGroup(groupName)) =>
          val pagination = Bukkit.getServicesManager.load(classOf[PaginationService])

          val title = t"$Yellow${groupName.capitalize} warps"
          val content = warpStorage.getGroupWarps(groupName).toSeq.sortBy(_._1).collect {
            case (name, warp) if warpStorage.canUseWarp(sender, warp) =>
              val btn = button(t"$Yellow${warp.textDisplayName}", s"/warp $name")
              warp.lore.fold(btn)(lore => t"$btn - $lore")
          }

          pagination
            .copyObj(title = Some(title), content = if (content.isEmpty) Seq(t"${Red}No warps found") else content)
            .sendTo(sender)

          Right(())
        }
      )
    )

  def warps(
      helpExecution: Executions,
      reloadData: () => Unit
  )(using warpStorage: WarpStorage, plugin: WarpsPlugin, ec: ExecutionContext, config: WarpsConfig): Command =
    Command("warps")(
      asyncExecution(
        "reload".asParameter,
        Senders.commandSender,
        LibPerm.Reload,
        description = _ => Some(t"Reload warps and configs")
      ) { case (sender, _) =>
        FutureOrNow.fromFuture(
          Future {
            Try(reloadData()) match {
              case Success(()) =>
                sender.sendMessage(t"${Green}Reload success")
              case Failure(e) =>
                sender.sendMessage(t"$Red${e.getMessage}")
            }

            Right(())
          }
        )
      },
      subCommand("help")(
        helpExecution
      ),
      asyncExecution(
        "set" ~> Parameters.string ~ optional(Parameters.string),
        Senders.player,
        LibPerm.Set,
        description = _ => Some(t"Set a new warp")
      ) { case (player, warpName ~ optGroup) =>
        warpStorage
          .setWarp(
            Warp.fromLocation(warpName, player.getLocation).copy(groups = optGroup.toSeq)
          )
          .map { _ =>
            player.sendMessage(t"${Green}Set warp $Aqua$warpName")
            Right(())
          }
      },
      warpListExecution
    )

  def warp(
      helpExecution: Executions
  )(using warpStorage: WarpStorage, plugin: ScalaPlugin, ec: ExecutionContext, teleporter: Teleporter): Command =
    Command("warp")(
      subCommand("help")(
        helpExecution
      ),
      subCommand("list")(
        warpListExecution
      ),
      withArgCached(warpParameter) { param =>
        NonEmptyList.of(
          execution(param, Senders.player, LibPerm.Teleport, description = _ => Some(t"Teleport to a warp")) {
            case (player, warp) =>
              teleporter
                .teleport(GlobalPlayer.OnThisServer(player), warp.locationWithoutWorld, warp.world, warp.server)
                .map { _ =>
                  player.sendMessage(t"${Green}Teleported to $Aqua${warp.textDisplayName}")
                  ()
                }
          },
          execution(
            param ~ "send" ~ Parameters.player,
            Senders.commandSender,
            permissions = LibPerm.Send,
            description = _ => Some(t"Send a player to a warp")
          ) { case (sender, warp ~ _ ~ target) =>
            teleporter
              .teleport(GlobalPlayer.OnThisServer(target), warp.locationWithoutWorld, warp.world, warp.server)
              .map { _ =>
                sender.sendMessage(t"${Green}Sent ${target.getName} to $Aqua${warp.name}")
                ()
              }
          },
          withArg(param <~ "allowed") { param =>
            def allowExecution(name: String, description: Text, success: Text => Text)(
                action: (Warp, Set[UUID], Set[String]) => Warp
            ) = {
              asyncExecution(
                param ~ name ~ userOrGroupParameter,
                Senders.commandSender,
                LibPerm.ModifyAllowed,
                description = _ => Some(description)
              ) { case (sender, warp ~ _ ~ userOrGroup) =>
                val (users, groups) = userOrGroup match {
                  case UserOrGroup.User(user)   => (Seq(user), Nil)
                  case UserOrGroup.Group(group) => (Nil, Seq(group))
                }

                warpStorage.setWarp(action(warp, users.map(_.getUniqueId).toSet, groups.toSet)).map { _ =>
                  sender.sendMessage(success(warp.textDisplayName))
                  Right(())
                }
              }
            }

            NonEmptyList.of(
              allowExecution("add", t"Add allowed to warp", name => t"${Green}Added allowed to $Aqua$name") {
                (warp, users, groups) =>
                  warp.copy(
                    allowedUsers = warp.allowedUsers.concat(users).distinct,
                    allowedPermGroups = warp.allowedPermGroups.concat(groups).distinct
                  )
              },
              allowExecution(
                "remove",
                t"Remove allowed from warp",
                name => t"${Green}Removed allowed from $Aqua$name"
              ) { (warp, users, groups) =>
                warp.copy(
                  allowedUsers = (warp.allowedUsers.toSet -- users).toSeq,
                  allowedPermGroups = (warp.allowedPermGroups.toSet -- groups).toSeq
                )
              }
            )
          },
          withArg(param <~ "groups") { param =>
            def groupExecution(name: String, description: Text, success: Text => Text)(
                action: (Warp, String) => Warp
            ) =
              asyncExecution(
                param ~ name ~ warpGroupParameter,
                Senders.commandSender,
                LibPerm.ModifyGroup,
                description = _ => Some(description)
              ) { case (sender, warp ~ _ ~ group) =>
                val stringGroup = group.name

                if (stringGroup == "all") {
                  FutureOrNow.now(Left("""Illegal group name "all""""))
                } else {
                  warpStorage.setWarp(action(warp, stringGroup)).map { _ =>
                    sender.sendMessage(success(warp.textDisplayName))
                    Right(())
                  }
                }
              }

            NonEmptyList.of(
              groupExecution(
                "add",
                t"Add group(s) to a warp",
                name => t"${Green}Added group to $Aqua$name"
              ) { (warp, group) =>
                warp.copy(groups = (warp.groups :+ group).distinct)
              },
              groupExecution(
                "remove",
                t"Remove group(s) from a warp",
                name => t"${Green}Removed group from $Aqua$name"
              ) { (warp, group) =>
                warp.copy(groups = warp.groups.filter(_ != group))
              }
            )
          },
          asyncExecution(
            param ~ "description" ~ Parameters.remainingAsString,
            Senders.commandSender,
            permissions = LibPerm.SetLore,
            description = _ => Some(t"Set the description of a warp")
          ) { case (sender, warp ~ _ ~ newDescription) =>
            warpStorage
              .setWarp(
                warp.copy(
                  lore =
                    if (newDescription.isEmpty) None
                    else
                      Some(LegacyComponentSerializer.legacyAmpersand().deserialize(newDescription))
                )
              )
              .map { _ =>
                sender.sendMessage(t"${Green}Set new description for $Aqua${warp.textDisplayName}")
                Right(())
              }
          },
          asyncExecution(
            param ~ "descriptionmini" ~ Parameters.remainingAsString,
            Senders.commandSender,
            permissions = LibPerm.SetLore,
            description = _ => Some(t"Set the description of a warp using MiniMessage")
          ) { case (sender, warp ~ _ ~ newDescription) =>
            warpStorage
              .setWarp(
                warp.copy(
                  lore =
                    if (newDescription.isEmpty) None
                    else
                      Some(MiniMessage.miniMessage().deserialize(newDescription, Nil*))
                )
              )
              .map { _ =>
                sender.sendMessage(t"${Green}Set new description for $Aqua${warp.textDisplayName}")
                Right(())
              }
          },
          execution(
            param ~ "rename" ~ Parameters.string,
            Senders.commandSender,
            permissions = LibPerm.Rename,
            description = _ => Some(t"Rename a warp")
          ) { case (sender, warp ~ _ ~ newName) =>
            warpStorage.setWarp(warp.copy(name = newName))
            sender.sendMessage(t"${Green}Renamed $Aqua${warp.name}$Green to $Aqua$newName")
            Right(())
          },
          asyncExecution(
            param ~ "displayname" ~ Parameters.remainingAsString,
            Senders.commandSender,
            permissions = LibPerm.SetDisplayName,
            description = _ => Some(t"Set the display name of a warp")
          ) { case (sender, warp ~ _ ~ newDisplayNameRaw) =>
            val newDisplayName =
              if (newDisplayNameRaw.isEmpty) None
              else Some(LegacyComponentSerializer.legacyAmpersand().deserialize(newDisplayNameRaw))
            warpStorage.setWarp(warp.copy(displayName = newDisplayName)).map { _ =>
              sender.sendMessage(
                t"${Green}Set display name of $Aqua${warp.name}$Green to $Aqua${newDisplayName.getOrElse(t"None")}"
              )
              Right(())
            }
          },
          asyncExecution(
            param ~ "displaynamemini" ~ Parameters.remainingAsString,
            Senders.commandSender,
            permissions = LibPerm.SetDisplayName,
            description = _ => Some(t"Set the display name of a warp using MiniMessage")
          ) { case (sender, warp ~ _ ~ newDisplayNameRaw) =>
            val newDisplayName =
              if (newDisplayNameRaw.isEmpty) None
              else Some(MiniMessage.miniMessage().deserialize(newDisplayNameRaw, Nil*))
            warpStorage.setWarp(warp.copy(displayName = newDisplayName)).map { _ =>
              sender.sendMessage(
                t"${Green}Set display name of $Aqua${warp.name}$Green to $Aqua${newDisplayName.getOrElse(t"None")}"
              )
              Right(())
            }
          },
          execution(
            param <~ "remove",
            Senders.commandSender,
            permissions = LibPerm.Remove,
            description = _ => Some(t"Remove a warp")
          ) { case (sender, warp) =>
            warpStorage.removeWarp(warp.name)
            sender.sendMessage(t"${Green}Removed $Aqua${warp.textDisplayName}")
            Right(())
          },
          execution(
            param <~ "info",
            permissions = LibPerm.WarpInfo,
            description = _ => Some(t"Get info about a warp")
          ) { case (sender, warp) =>
            val pagination = Bukkit.getServicesManager.load(classOf[PaginationService])

            val commandName = t"Warp name: $Yellow${warp.name}"
            val displayName = t"Warp display name: $Yellow${warp.displayName.getOrElse(t"None")}"
            val groups      = warp.groups

            val numberFormatter = NumberFormat.getNumberInstance
            val x               = numberFormatter.format(warp.x)
            val y               = numberFormatter.format(warp.y)
            val z               = numberFormatter.format(warp.z)
            val yaw             = numberFormatter.format(warp.yaw)
            val pitch           = numberFormatter.format(warp.pitch)

            val coordsText   = t"X:${t"$Yellow$x"} Y:${t"$Yellow$y"} Z:${t"$Yellow$z"}"
            val rotationText = t"Yaw:${t"$Yellow$yaw"} Pitch:${t"$Yellow$pitch"}"
            val worldText = Option(Bukkit.getWorld(warp.world))
              .map(w => t"World: $Yellow${w.getName}")
              .getOrElse(t"World: ${Yellow}Unknown")

            val groupsText =
              if groups.isEmpty
              then t"Groups: ${Yellow}None"
              else t"Groups: $Yellow${groups.mkString(", ")}"

            val allowedPermGroupsText =
              if warp.allowedPermGroups.nonEmpty
              then t"Allowed groups: $Yellow${warp.allowedPermGroups.mkString(", ")}"
              else t"Allowed groups: ${Yellow}All"

            val allowedUsersText =
              if warp.allowedUsers.nonEmpty
              then
                t"Allowed users: $Yellow${warp.allowedUsers
                    .flatMap(uuid => Bukkit.getOfflinePlayers.find(_.getUniqueId == uuid).map(_.getName))
                    .mkString(", ")}"
              else t"Allowed users: ${Yellow}All"

            val loreText = warp.lore.map(lore => t"Lore: $lore").getOrElse(t"Lore: None")

            pagination
              .copyObj(
                title = Some(t"$Yellow${warp.textDisplayName} info"),
                content = Seq(
                  commandName,
                  displayName,
                  coordsText,
                  rotationText,
                  worldText,
                  groupsText,
                  allowedPermGroupsText,
                  allowedUsersText,
                  loreText
                )
              )
              .sendTo(sender)
            Right(())
          }
        )
      }
    )
}
