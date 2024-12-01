package net.katsstuff.bukkit.homesweethome.cmd

import java.util.{Locale, UUID}

import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.jdk.CollectionConverters.*
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

import cats.data.{EitherT, NonEmptyList, StateT}
import cats.syntax.all.*
import io.papermc.paper.command.brigadier.argument.ArgumentTypes
import io.papermc.paper.command.brigadier.argument.resolvers.PlayerProfileListResolver
import net.katsstuff.bukkit.homesweethome.*
import net.katsstuff.bukkit.homesweethome.home.Home
import net.katsstuff.bukkit.homesweethome.home.homehandler.HomeHandler
import net.katsstuff.bukkit.homesweethome.lib.LibPerm
import net.katsstuff.bukkit.katlib.command.*
import net.katsstuff.bukkit.katlib.service.PaginationService
import net.katsstuff.bukkit.katlib.text.*
import net.katsstuff.bukkit.katlib.util.{FutureOrNow, Teleporter}
import net.katsstuff.bukkit.katlib.{BungeeChannel, GlobalPlayer, ScalaPlugin}
import net.kyori.adventure.audience.Audience
import net.kyori.adventure.text.Component
import net.kyori.adventure.text.event.{ClickCallback, ClickEvent}
import org.bukkit.command.CommandSender
import org.bukkit.entity.{Entity, Player}
import org.bukkit.{Bukkit, Location, OfflinePlayer, World}

//noinspection UnstableApiUsage
object HomeCommands:

  inline def lowercase(s: String): String = s.toLowerCase(Locale.ROOT)

  def homeOwner(using homeHandler: HomeHandler): Parameter[OfflinePlayer] = Parameter.choicesSingleMap(
    "home-owner",
    homeHandler.homeOwnersPlayers
  )

  def globalPlayer(using homeHandler: HomeHandler): Parameter[GlobalPlayer] =
    Parameter.choicesSingleOpt(
      "player",
      input => homeHandler.globalOnlinePlayers.find(_.name == input),
      homeHandler.globalOnlinePlayers.map(_.name),
      showChoicesInUsage = false
    )

  def homeRequester(using homeHandler: HomeHandler): Parameter[(GlobalPlayer, Home)] =
    new Parameter.ChoicesSingleSyncParameter[(GlobalPlayer, Home)](
      "home-requester",
      getValue = {
        case (player: Player, input) =>
          Option(Bukkit.getOfflinePlayerIfCached(input)).flatMap { offline =>
            homeHandler.getRequest(offline.getUniqueId, player.getUniqueId).map(GlobalPlayer.ofOffline(offline) -> _)
          }
        case (_, _) => None
      },
      suggestionsChoices = {
        case player: Player =>
          homeHandler
            .getAllRequestersForPlayer(player.getUniqueId)
            .map(Bukkit.getOfflinePlayer)
            .filter(_.getName != null)
            .map(_.getName)
        case _ => Nil
      }
    )

  def manyHomes(
      owner: Option[OfflinePlayer]
  )(using homeHandler: HomeHandler, ec: ExecutionContext): Parameter[Set[HomeWithName]] =
    new Parameter.ChoicesManyParameter[HomeWithName](
      "home",
      choices = sender => {
        inline def cast[A](obj: AnyRef): Option[A] = obj match
          case a: A => Some(a)
          case _    => None

        owner.orElse(cast[OfflinePlayer](sender)) match
          case Some(player) =>
            homeHandler.allHomesForPlayer(player.getUniqueId).map(_.map(t => t._1 -> HomeWithName(t._1, t._2)))
          case None => FutureOrNow.now(Map.empty)
      }
    )

  def homeWithOwner(
      owner: Option[OfflinePlayer]
  )(using HomeHandler, ExecutionContext): Parameter[HomeWithName] =
    single(manyHomes(owner))

  def home(using HomeHandler, ExecutionContext): Parameter[HomeWithName] =
    homeWithOwner(None)

  def otherHome(using homeHandler: HomeHandler, ec: ExecutionContext): Parameter[OtherHome] =
    new Parameter[OtherHome]:
      val homeOwnerParam: Parameter[OfflinePlayer] = homeOwner
      val stringParam: Parameter[String]           = Parameters.string.named("home")

      val param: Parameter[OtherHome] = (homeOwnerParam ~ stringParam).aemap { (owner, name) =>
        homeHandler
          .specificHome(owner.getUniqueId, name)
          .map(
            _.toRight(CommandError(s"""No home named "$name" for ${owner.getName} found"""))
              .map(home => OtherHome(isOther = true, HomeWithName(name, home), owner))
          )
      }

      override def parse(source: CommandSender)(
          using ExecutionContext
      ): EitherT[ParameterState, CommandFailure, OtherHome] = param.parse(source)

      override def suggestions(source: CommandSender)(using ExecutionContext): ParameterState[List[String]] =
        StateT { (s: List[RawCmdArg]) =>
          val firstParse = homeOwnerParam.parse(source).value.run(s)
          firstParse.flatMap {
            case (_, Left(_))       => homeOwnerParam.suggestions(source).run(s)
            case (Nil, Right(_))    => homeOwnerParam.suggestions(source).run(s)
            case (s2, Right(owner)) => homeWithOwner(Some(owner)).suggestions(source).run(s2)
          }
        }

      override def usage(source: CommandSender): Usage = param.usage(source)
  end otherHome

  def makeHomeSearchQueryParameter[A, B](
      prefix: String,
      param: Parameter[A],
      f: A => CommandResult[HomeSearchQuery[B]]
  ): Parameter[HomeSearchQuery[?]] =
    prefix.asParameter ~> param.emap(a => f(a).asInstanceOf[CommandResult[HomeSearchQuery[?]]])

  val homeSearchQueryRadiusParameter: Parameter[HomeSearchQuery[?]] =
    makeHomeSearchQueryParameter("r", Parameters.double, r => Right(HomeSearchQuery.Radius(r)))
  val homeSearchQueryWorldParameter: Parameter[HomeSearchQuery[?]] =
    makeHomeSearchQueryParameter("w", Parameters.world, w => Right(HomeSearchQuery.World(w)))
  def homeSearchQueryOwnerParameter(using HomeHandler): Parameter[HomeSearchQuery[?]] =
    makeHomeSearchQueryParameter("o", homeOwner, p => Right(HomeSearchQuery.Owner(p.getUniqueId)))

  def homeSearchParameter(using HomeHandler): Parameter[HomeSearchQuery[?]] =
    homeSearchQueryRadiusParameter | homeSearchQueryWorldParameter | homeSearchQueryOwnerParameter

  val locationSender: UserValidator[Location] =
    case entity: Entity => Right(entity.getLocation)
    case _              => Left(CommandUsageError("User needs to have a location", -1))

  def listExecution(using HomeHandler, ScalaPlugin, ExecutionContext): Executions =
    asyncExecution(
      sender = Senders.player,
      permissions = LibPerm.HomeList,
      description = _ => Some(t"Lists all of your current homes")
    ) { case (sender, _) =>
      listHomes(sender, sender, isOther = false)
    }

  def homesCommand(using HomeHandler, HomePlugin, ExecutionContext): Command =
    Command("homes")(
      listExecution
    )

  def homeCommand(
      helpExecution: Executions,
      reloadData: () => Unit
  )(
      using homeHandler: HomeHandler,
      plugin: HomePlugin,
      ec: ExecutionContext,
      bungeeChannel: BungeeChannel,
      teleporter: Teleporter
  ): Command =
    Command("home")(
      asyncExecution(
        "set" ~> Parameters.string.named("home"),
        Senders.player,
        permissions = LibPerm.HomeSet,
        description = _ => Some(t"Set a new home where you are standing")
      ) { case (sender, homeName) =>
        setHome(sender, sender, homeName, isOther = false)
      },
      subCommand("list")(
        listExecution
      ),
      subCommand("other")(
        withArgCached(homeOwner) { arg =>
          NonEmptyList.of(
            asyncExecution(
              arg ~ "set" ~ Parameters.string.named("home"),
              Senders.player,
              permissions = LibPerm.HomeOtherSet,
              description = _ => Some(t"Set a new home for someone else where you are standing")
            ) { case (sender, owner ~ _ ~ homeName) =>
              setHome(sender, owner, homeName, isOther = true)
            },
            asyncExecution(
              arg <~ "list",
              Senders.player,
              permissions = LibPerm.HomeOtherList,
              description = _ => Some(t"Lists all of someone's current homes")
            ) { case (sender, owner) =>
              listHomes(sender, owner, isOther = true)
            },
            asyncExecution(
              arg <~ "residents",
              Senders.player,
              permissions = LibPerm.HomeOtherResidentList,
              description = _ => Some(t"List all residents for someone")
            ) { case (sender, owner) =>
              listResidents(sender, owner, isOther = true)
            }
          )
        },
        withArgCached(otherHome) { arg =>
          NonEmptyList.of(
            execution(
              arg,
              Senders.player,
              permissions = LibPerm.HomeOtherTp,
              description = _ => Some(t"Teleport to one of someone else's homes")
            ) { case (sender, home) =>
              tpHome(sender, home)
            },
            asyncExecution(
              arg <~ "delete",
              permissions = LibPerm.HomeOtherDelete,
              description = _ => Some(t"Deletes someone else's home")
            ) { case (sender, home) =>
              deleteHome(sender, home)
            },
            execution(
              arg ~ "invite" ~ globalPlayer,
              permissions = LibPerm.HomeOtherInvite,
              description = _ => Some(t"Invite someone to someone's home")
            ) { case (sender, home ~ _ ~ player) =>
              inviteToHome(sender, home, player)
            },
            withArg(arg <~ "residents") { arg =>
              NonEmptyList.of(
                asyncExecution(
                  arg,
                  permissions = LibPerm.HomeOtherResidentList,
                  description = _ => Some(t"List the residents of a home")
                ) { case (sender, home) =>
                  listHomeResidents(sender, home)
                },
                asyncExecution(
                  arg ~ "add" ~ single(Parameters.offlinePlayers),
                  permissions = LibPerm.HomeOtherResidentAdd,
                  description = _ => Some(t"Add a user as a resident to someone's home")
                ) { case (sender, home ~ _ ~ player) =>
                  addResident(sender, home, player)
                },
                asyncExecution(
                  arg ~ "remove" ~ single(Parameters.offlinePlayers),
                  permissions = LibPerm.HomeOtherResidentRemove,
                  description = _ => Some(t"Remove a user as a resident from someone's home")
                ) { case (sender, home ~ _ ~ player) =>
                  removeResident(sender, home, player)
                }
              )
            }
          )
        }
      ),
      execution(
        "accept" ~> homeRequester,
        Senders.player,
        permissions = LibPerm.HomeAccept,
        description = _ => Some(t"Accept a home request")
      ) { case (homeOwner, (requester, home)) =>
        teleporter.teleport(requester, home.locationWithoutWorld, home.worldUuid, home.server).map { _ =>
          requester.sendMessage(homeOwner, t"${Yellow}Teleported you to your requested home")
          homeOwner.sendMessage(t"${Green}Teleported ${requester.name} to their requested home")
          homeHandler.removeRequest(requester.uuid, homeOwner.getUniqueId)
          ()
        }
      },
      asyncExecution(
        "goto" ~> homeOwner ~ Parameters.string,
        Senders.player,
        permissions = LibPerm.HomeGoto,
        description = _ => Some(t"Go to another players home if you are allowed to go there")
      ) { case (player, homeOwner ~ homeName) =>
        homeHandler
          .specificHome(homeOwner.getUniqueId, homeName)
          .zip(homeHandler.isPlayerResident(homeOwner.getUniqueId, homeName, player.getUniqueId))
          .map((homeOpt, isResident) =>
            homeOpt.toRight(HomeNotFound).flatMap { home =>
              val homeOwnerOnline = homeHandler.globalOnlinePlayers.exists(_.uuid == homeOwner.getUniqueId)

              val isInvited  = homeHandler.isInvited(player.getUniqueId, homeOwner.getUniqueId, home) && homeOwnerOnline
              val canUseGoto = isResident || isInvited

              if canUseGoto then
                teleporter
                  .teleport(GlobalPlayer.OnThisServer(player), home.locationWithoutWorld, home.worldUuid, home.server)
                  .map { _ =>
                    player.sendMessage(t"""${Green}Teleported to "$homeName" for ${homeOwner.getName}""")
                    homeHandler.removeInvite(player.getUniqueId, homeOwner.getUniqueId)
                    ()
                  }
              else if homeOwnerOnline then
                player.sendMessage(t"""${Green}Sent home request to ${homeOwner.getName} for "$homeName"""")
                homeHandler.addRequest(player.getUniqueId, homeOwner.getUniqueId, home).foreach { _ =>
                  val acceptButton = button(t"${Yellow}Accept", s"/home accept ${player.getName}")

                  GlobalPlayer
                    .ofOffline(homeOwner)
                    .sendMessage(
                      player,
                      t"${t"""$Yellow${player.getName} has requested a to be teleported to "$homeName".${Component
                            .newline()}"""}$acceptButton"
                    )
                }

                Right(())
              else Left("The player you tried to send a home request to is offline")
            }
          )
      },
      asyncExecution(
        "residents".asParameter,
        Senders.player,
        permissions = LibPerm.HomeResidentList,
        description = _ => Some(t"List all your residents")
      ) { case (sender, _) =>
        listResidents(sender, sender, isOther = false)
      },
      subCommand("help")(
        helpExecution
      ),
      subCommand("mod")(
        asyncExecution(
          "search".asParameter ~> (homeSearchParameter ~ optional(homeSearchParameter) ~ optional(homeSearchParameter))
            .map(t => Seq(t._1._1) ++ t._1._2 ++ t._2),
          Senders.player,
          permissions = LibPerm.Search,
          description = _ => Some(t"Search for homes")
        ) { case (player, searchQueries) =>
          val params: Map[Class[? <: HomeSearchQuery[?]], HomeSearchQuery[?]] =
            searchQueries.toList.map(q => q.getClass -> q).toMap // One query per property

          println(s"Search executed $params")

          inline def getProp[Q <: HomeSearchQuery[?]](using tag: ClassTag[Q]): Option[HomeSearchQuery.Tpe[Q]] =
            params.get(tag.runtimeClass.asInstanceOf[Class[Q]]).map(_.value.asInstanceOf[HomeSearchQuery.Tpe[Q]])

          homeHandler
            .searchHomes(
              player.getLocation,
              radius = getProp[HomeSearchQuery.Radius],
              world = getProp[HomeSearchQuery.World].map((w: World) => w.getUID),
              owner = getProp[HomeSearchQuery.Owner],
              drop = 0,
              take = -1
            )
            .map { (homes: Seq[Home]) =>
              val paginationBase = Bukkit.getServicesManager.load(classOf[PaginationService])

              def df(d: Double): String = f"$d%.2f"

              println(s"Home search result $homes")

              val body = homes.flatMap { home =>
                def teleportButton =
                  t"[${t"${Yellow}Teleport"}]".clickEvent(
                    ClickEvent.callback(
                      (_: Audience) => {
                        teleporter
                          .teleport(
                            GlobalPlayer.OnThisServer(player),
                            home.locationWithoutWorld,
                            home.worldUuid,
                            home.server
                          )
                          .swap
                          .foreach { e =>
                            player.sendMessage(t"${TextColor.Red}$e")
                          }
                      },
                      ClickCallback.Options.builder().uses(-1).build()
                    )
                  )

                val ownerName = Option(Bukkit.getOfflinePlayer(home.owner).getName).getOrElse("unknown")
                val worldName = home.world.fold("unknown")(_.getName)

                Seq(
                  t"$ownerName/${home.name} $teleportButton",
                  t"OwnerUUID: ${home.owner.toString}",
                  t"X: ${df(home.x)} Y: ${df(home.y)} Z: ${df(home.z)} World: $worldName"
                )
              }

              if body.nonEmpty then
                paginationBase
                  .copyObj(
                    title = Some(t"Home search"),
                    linesPerPage = 20,
                    content = body
                  )
                  .sendTo(player)
              else player.sendMessage(t"${Yellow}No homes found with these parameters")

              Right(())
            }
        }
      ),
      subCommand("admin")(
        asyncExecution(
          "reload".asParameter,
          Senders.commandSender,
          permissions = LibPerm.Reload,
          description = _ => Some(t"Reloads stuff")
        ) { case (sender, _) =>
          FutureOrNow.fromFuture(
            Future {
              Try(reloadData()) match {
                case Success(()) =>
                  sender.sendMessage(t"${Green}Reload success")
                case Failure(e) =>
                  e.printStackTrace()
                  sender.sendMessage(t"$Red${e.getMessage}")
              }

              Right(())
            }
          )
        },
        asyncExecution(
          "export".asParameter,
          Senders.commandSender,
          permissions = LibPerm.Export,
          description = _ => Some(t"Export home data")
        ) { case (sender, _) =>
          homeHandler.exportStorageData().map { _ =>
            sender.sendMessage(t"${Green}Export successful")
            Right(())
          }
        },
        asyncExecution(
          "import".asParameter,
          Senders.commandSender,
          permissions = LibPerm.Import,
          description = _ => Some(t"Import home data")
        ) { case (sender, _) =>
          homeHandler.importStorageData().map { _ =>
            sender.sendMessage(t"${Green}Import successful")
            Right(())
          }
        }
      ),
      withArgCached(home) { arg =>
        NonEmptyList.of(
          execution(
            arg,
            Senders.player,
            permissions = LibPerm.HomeTp,
            description = _ => Some(t"Teleport to one of your homes")
          ) { case (sender, home) =>
            tpHome(sender, OtherHome.same(home, sender))
          },
          asyncExecution(
            arg <~ "delete",
            Senders.player,
            permissions = LibPerm.HomeDelete,
            description = _ => Some(t"Deletes a home")
          ) { case (sender, home) =>
            deleteHome(sender, OtherHome.same(home, sender))
          },
          execution(
            arg ~ "invite" ~ globalPlayer,
            Senders.player,
            permissions = LibPerm.HomeInvite,
            description = _ => Some(t"Invite someone to your home")
          ) { case (sender, home ~ _ ~ player) =>
            inviteToHome(sender, OtherHome.same(home, sender), player)
          },
          withArg(arg <~ "residents") { arg =>
            NonEmptyList.of(
              asyncExecution(
                arg,
                Senders.player,
                permissions = LibPerm.HomeResidentList,
                description = _ => Some(t"List the residents of a home")
              ) { case (sender, home) =>
                listHomeResidents(sender, OtherHome.same(home, sender))
              },
              asyncExecution(
                arg ~ "add" ~ single(Parameters.offlinePlayers),
                Senders.player,
                permissions = LibPerm.HomeResidentAdd,
                description = _ => Some(t"Add a user as a resident to a home")
              ) { case (sender, home ~ _ ~ player) =>
                addResident(sender, OtherHome.same(home, sender), player)
              },
              asyncExecution(
                arg ~ "remove" ~ single(Parameters.offlinePlayers),
                Senders.player,
                permissions = LibPerm.HomeResidentRemove,
                description = _ => Some(t"Remove a user as a resident from a home")
              ) { case (sender, home ~ _ ~ player) =>
                removeResident(sender, OtherHome.same(home, sender), player)
              }
            )
          }
        )
      }
    )

  def listHomes(sender: CommandSender, owner: OfflinePlayer, isOther: Boolean)(
      using homeHandler: HomeHandler,
      plugin: ScalaPlugin,
      ec: ExecutionContext
  ): FutureOrNow[Either[String, Unit]] =
    homeHandler.allHomesForPlayer(owner.getUniqueId).map(_.keys.toSeq).map { homes =>
      val world = locationSender.validate(sender).fold(_ => Bukkit.getWorlds.get(0), _.getWorld)
      val limit = homeHandler.getHomeLimit(world, owner)

      if homes.isEmpty then
        if isOther
        then sender.sendMessage(t"${Yellow}Limit: $limit${Component.newline()}${owner.getName} don't have any homes.")
        else sender.sendMessage(t"${Yellow}Limit: $limit${Component.newline()}You don't have any homes.")
      else
        val paginationBase = Bukkit.getServicesManager.load(classOf[PaginationService])
        val commandPrefix  = if isOther then s"/home other ${owner.getName}" else "/home"

        val homeText = homes.sorted.map { rawHomeName =>
          val homeName = rawHomeName.replace("""\""", """\\""")

          val teleportButton = button(t"${Yellow}Teleport", s"""$commandPrefix "$homeName"""")
          val setButton      = confirmButton(t"${Yellow}Set", s"""$commandPrefix set "$homeName"""")
          val inviteButton   = confirmButton(t"${Yellow}Invite", s"""$commandPrefix "$homeName" invite <player>""")
          val deleteButton   = confirmButton(t"${Red}Delete", s"""$commandPrefix "$homeName" delete""")

          val residentsButton = button(t"${Yellow}Residents", s"""$commandPrefix "$homeName" residents""")

          t""""$homeName" $teleportButton $setButton $inviteButton $residentsButton $deleteButton"""
        }

        val limitText = t"Limit: $limit"
        val newButton = confirmButton(t"${Yellow}New home", s"$commandPrefix set <homeName>")

        paginationBase
          .copyObj(title = Some(t"${Yellow}Homes"), content = limitText +: newButton +: homeText)
          .sendTo(sender)
      end if

      Right(())
    }
  end listHomes

  val disallowedHomeNames: Seq[String] = Seq(
    "set",
    "list",
    "other",
    "accept",
    "goto",
    "residents",
    "help",
    "mod",
    "admin"
  )

  def setHome(sender: Player, owner: OfflinePlayer, name: String, isOther: Boolean)(
      using homeHandler: HomeHandler,
      plugin: ScalaPlugin,
      ec: ExecutionContext
  ): FutureOrNow[Either[String, Unit]] =
    if disallowedHomeNames.exists(lowercase(name).startsWith)
    then FutureOrNow.now(Left("That home name is disallowed"))
    else
      homeHandler.homeExist(owner.getUniqueId, name).zip(homeHandler.homeCount(owner.getUniqueId)).flatMap {
        (replace, homeCount) =>
          if
            // Scalafmt unindents this block
            // format:off
            val limit            = homeHandler.getHomeLimit(sender.getWorld, owner)
            val limitWithReplace = if replace then limit + 1 else limit
            val limitReached     = homeCount >= limitWithReplace
            limitReached
            // format:on
          then FutureOrNow.now(Left("Home limit reached"))
          else
            homeHandler.makeHome(owner.getUniqueId, name, sender.getLocation).map { _ =>
              val homeNameText =
                if isOther
                then s""""$name" for ${owner.getName}"""
                else s""""$name""""

              sender.sendMessage(t"${Green}Set $homeNameText successfully")
              Right(())
            }
      }
  end setHome

  def tpHome(sender: Player, home: OtherHome)(
      using plugin: HomePlugin,
      teleporter: Teleporter
  ): Either[String, Unit] =
    teleporter
      .teleport(
        GlobalPlayer.OnThisServer(sender),
        home.home.locationWithoutWorld,
        home.home.worldUuid,
        home.home.server
      )
      .map { _ =>
        sender.sendMessage(t"${Green}Teleported to ${home.chatHomeName} successfully")
      }

  def deleteHome(
      sender: CommandSender,
      home: OtherHome
  )(using homeHandler: HomeHandler, plugin: ScalaPlugin, ec: ExecutionContext): FutureOrNow[Either[String, Unit]] =
    homeHandler.deleteHome(home.homeOwner.getUniqueId, home.namedHome.name).map { _ =>
      sender.sendMessage(t"${Green}Deleted ${home.chatHomeName}")
      Right(())
    }

  def inviteToHome(sender: CommandSender, home: OtherHome, player: GlobalPlayer)(
      using homeHandler: HomeHandler,
      plugin: ScalaPlugin,
      bungeeChannel: BungeeChannel,
      ec: ExecutionContext
  ): Either[String, Unit] =
    val playerSender = sender match
      case sender: Player                                => Right(sender)
      case _ if Bukkit.getOnlinePlayers.asScala.nonEmpty => Right(Bukkit.getOnlinePlayers.asScala.head)
      case _ => Left("Can't invite player on another server if there are not players present at the origin")

    homeHandler.addInvite(player.uuid, home.homeOwner.getUniqueId, home.home).foreach { _ =>
      val gotoButton =
        button(t"${Yellow}Go to ${home.chatHomeName}", s"/home goto ${home.homeOwner.getName} ${home.namedHome.name}")

      playerSender.foreach { p =>
        player.sendMessage(
          p,
          t"${t"${Yellow}You have been invited to ${home.chatHomeName} by ${sender.getName}${Component.newline()}"}$gotoButton"
        )
      }
    }

    playerSender.map { _ =>
      sender.sendMessage(t"${Green}Invited ${player.name} to ${home.chatHomeName}")
    }

  def listHomeResidents(sender: CommandSender, home: OtherHome)(
      using homeHandler: HomeHandler,
      ec: ExecutionContext
  ): FutureOrNow[Either[String, Unit]] =
    homeHandler.getHomeResidents(home.homeOwner.getUniqueId, home.name).map(_.toSeq).map { residents =>
      val homeName    = home.namedHome.name.replace("""\""", """\\""")
      val otherPrefix = if (home.isOther) s"/home other ${home.homeOwner.getName} $homeName" else s"/home $homeName"
      val world       = locationSender.validate(sender).fold(_ => Bukkit.getWorlds.get(0), _.getWorld)
      val limit       = homeHandler.getResidentLimit(world, home.homeOwner)

      val builder = Bukkit.getServicesManager.load(classOf[PaginationService])

      val title = t"""$Yellow"$homeName"'s residents"""

      val residentText =
        if residents.isEmpty
        then Seq(t"${Yellow}No residents")
        else
          residents
            .map(uuid => Option(Bukkit.getOfflinePlayer(uuid).getName).getOrElse(uuid.toString))
            .sorted
            .map { residentName =>
              val deleteButton =
                confirmButton(
                  t"${Red}Delete",
                  s"""$otherPrefix residents remove $residentName"""
                )

              t"$residentName $deleteButton"
            }

      val limitText = t"Limit: $limit"
      val newButton =
        confirmButton(t"${Yellow}New resident", s"""$otherPrefix residents add <player>""")

      builder.copyObj(title = Some(title), content = limitText +: newButton +: residentText).sendTo(sender)

      Right(())
    }
  end listHomeResidents

  def addResident(sender: CommandSender, home: OtherHome, player: OfflinePlayer)(
      using homeHandler: HomeHandler,
      plugin: ScalaPlugin,
      ec: ExecutionContext
  ): FutureOrNow[Either[String, Unit]] =
    homeHandler.getHomeResidents(home.homeOwner.getUniqueId, home.name).flatMap { residents =>
      val world           = locationSender.validate(sender).fold(_ => Bukkit.getWorlds.get(0), _.getWorld)
      val limitReached    = residents.size >= homeHandler.getResidentLimit(world, home.homeOwner)
      val alreadyResident = residents.contains(player.getUniqueId)

      if limitReached then FutureOrNow.now(Left("Resident limit reached"))
      else if alreadyResident then
        FutureOrNow.now(Left(s"${player.getName} is already a resident of ${home.chatHomeName}"))
      else
        homeHandler.addResident(home.homeOwner.getUniqueId, home.name, player.getUniqueId).map { _ =>
          sender.sendMessage(t"${Green}Adding ${player.getName} as a resident to ${home.chatHomeName}")
          Right(())
        }
    }
  end addResident

  def removeResident(sender: CommandSender, home: OtherHome, player: OfflinePlayer)(
      using homeHandler: HomeHandler,
      plugin: ScalaPlugin,
      ec: ExecutionContext
  ): FutureOrNow[Either[String, Unit]] =
    homeHandler.getHomeResidents(home.homeOwner.getUniqueId, home.name).flatMap { residents =>
      if !residents.contains(player.getUniqueId)
      then FutureOrNow.now(Left(s"""${player.getName} is not a resident of ${home.chatHomeName}"""))
      else
        homeHandler.removeResident(home.homeOwner.getUniqueId, home.name, player.getUniqueId).map { _ =>
          sender.sendMessage(t"${Green}Removed ${player.getName} as a resident from ${home.chatHomeName}")
          Right(())
        }
    }
  end removeResident

  def listResidents(sender: CommandSender, owner: OfflinePlayer, isOther: Boolean)(
      using homeHandler: HomeHandler,
      ec: ExecutionContext
  ): FutureOrNow[Either[String, Unit]] =
    val otherPrefix = if isOther then s"/home other ${owner.getName}" else "/home"
    val world       = locationSender.validate(sender).fold(_ => Bukkit.getWorlds.get(0), _.getWorld)
    val limit       = homeHandler.getResidentLimit(world, owner)

    val builder = Bukkit.getServicesManager.load(classOf[PaginationService])

    homeHandler.allResidentsForPlayer(owner.getUniqueId).map { residents =>
      val title = t"""$Yellow${owner.getName}'s residents"""

      val residentText =
        if residents.isEmpty
        then Seq(t"${Yellow}No homes")
        else
          residents.toSeq
            .sortBy(_._1)
            .map { case (homeName, homeResidentsUuids) =>
              val details =
                button(t"${Yellow}Details", s"""$otherPrefix "${homeName.replace("""\""", """\\""")}" residents""")

              if homeResidentsUuids.isEmpty
              then t"${t""""$homeName": ${Yellow}No residents"""} $details"
              else
                val homeResidents = homeResidentsUuids.map { uuid =>
                  Option(Bukkit.getOfflinePlayer(uuid).getName).getOrElse(uuid.toString)
                }
                t"${t""""$homeName": $Yellow${homeResidents.mkString(", ")}"""} $details"
            }

      val limitText = t"Limit: $limit"

      builder.copyObj(title = Some(title), content = limitText +: residentText).sendTo(sender)

      Right(())
    }
  end listResidents
