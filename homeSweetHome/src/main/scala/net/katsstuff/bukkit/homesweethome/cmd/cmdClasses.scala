package net.katsstuff.bukkit.homesweethome.cmd

import java.util.UUID

import net.katsstuff.bukkit.homesweethome.home.Home
import org.bukkit.entity.Player
import org.bukkit.{OfflinePlayer, World as BukkitWorld}

case class HomeWithName(name: String, home: Home)

case class OtherHome(isOther: Boolean, namedHome: HomeWithName, homeOwner: OfflinePlayer):
  def chatHomeName: String =
    if isOther
    then s""""${namedHome.name}" for ${homeOwner.getName}"""
    else s""""${namedHome.name}""""

  def home: Home = namedHome.home

  def name: String = namedHome.name

object OtherHome:
  def same(home: HomeWithName, sender: Player): OtherHome = OtherHome(isOther = false, home, sender)

enum HomeSearchQuery[A](val value: A):
  case Radius(r: Double)     extends HomeSearchQuery[Double](r)
  case World(w: BukkitWorld) extends HomeSearchQuery[BukkitWorld](w)
  case Owner(id: UUID)       extends HomeSearchQuery[UUID](id)

object HomeSearchQuery:
  type Tpe[Q <: HomeSearchQuery[?]] = Q match
    case Radius => Double
    case World  => BukkitWorld
    case Owner  => UUID
