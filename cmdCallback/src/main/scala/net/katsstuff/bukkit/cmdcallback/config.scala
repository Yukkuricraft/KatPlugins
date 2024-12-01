package net.katsstuff.bukkit.cmdcallback

import scala.jdk.CollectionConverters.*

import io.circe.Decoder
import io.circe.derivation.Configuration
import org.bukkit.entity.Player
import org.bukkit.inventory.{ItemStack, PlayerInventory}
import org.bukkit.potion.PotionEffect

given Configuration = Configuration.default.withDiscriminator("type")

trait Testable[Cond, V]:
  extension (self: Cond) def test(v: V): Boolean

case class EqCondition[V](equalTo: V) derives Decoder
object EqCondition:
  given [V](using e: Equiv[V]): Testable[EqCondition[V], V] with
    extension (self: EqCondition[V]) def test(v: V): Boolean = e.equiv(self.equalTo, v)

enum NumberCondition[V] derives Decoder:
  case Gt(value: V)
  case Ge(value: V)
  case Equal(value: V)
  case Le(value: V)
  case Lt(value: V)

object NumberCondition:
  given [V](using o: Ordering[V]): Testable[NumberCondition[V], V] with
    extension (self: NumberCondition[V])
      override def test(v: V): Boolean = self match
        case NumberCondition.Gt(value)    => o.gt(v, value)
        case NumberCondition.Ge(value)    => o.gteq(v, value)
        case NumberCondition.Equal(value) => o.equiv(v, value)
        case NumberCondition.Le(value)    => o.lteq(v, value)
        case NumberCondition.Lt(value)    => o.lt(v, value)

enum BoolCondition[Cond] derives Decoder:
  case And(lhs: BoolCondition[Cond], rhs: BoolCondition[Cond])
  case Or(lhs: BoolCondition[Cond], rhs: BoolCondition[Cond])
  case Not(cond: BoolCondition[Cond])
  case Test(cond: Cond)

object BoolCondition:
  given [Cond, V](using Testable[Cond, V]): Testable[BoolCondition[Cond], V] with
    extension (self: BoolCondition[Cond])
      def test(v: V): Boolean = self match
        case BoolCondition.And(lhs, rhs) => lhs.test(v) && rhs.test(v)
        case BoolCondition.Or(lhs, rhs)  => lhs.test(v) || rhs.test(v)
        case BoolCondition.Not(cond)     => !cond.test(v)
        case BoolCondition.Test(cond)    => cond.test(v)

enum PotionCondition derives Decoder:
  case Amplifier(cond: BoolCondition[NumberCondition[Int]])
  case Duration(cond: BoolCondition[NumberCondition[Int]])
  case Identifier(cond: BoolCondition[EqCondition[String]])

object PotionCondition:
  given Testable[PotionCondition, PotionEffect] with
    extension (self: PotionCondition)
      def test(v: PotionEffect): Boolean = self match
        case PotionCondition.Amplifier(cond)  => cond.test(v.getAmplifier)
        case PotionCondition.Duration(cond)   => cond.test(v.getDuration)
        case PotionCondition.Identifier(cond) => cond.test(v.getType.getKey.asString)

enum ItemCondition derives Decoder:
  case Identifier(cond: BoolCondition[EqCondition[String]])
  case Amount(cond: BoolCondition[NumberCondition[Int]])
  case Component(key: String) // Non-functional until we update to 1.21.3
  case IsNull

object ItemCondition:
  given Testable[ItemCondition, Option[ItemStack]] with
    extension (self: ItemCondition)
      def test(vo: Option[ItemStack]): Boolean =
        self match
          case ItemCondition.Identifier(cond) => vo.exists(v => cond.test(v.getType.getKey.asString))
          case ItemCondition.Amount(cond)     => vo.exists(v => cond.test(v.getAmount))
          case ItemCondition.Component(_)     => false
          case ItemCondition.IsNull           => vo.isEmpty

enum CollectionCondition[Cond] derives Decoder:
  case Any(cond: BoolCondition[Cond])
  case Every(cond: BoolCondition[Cond])

object CollectionCondition:
  given [Cond, V](using Testable[Cond, V]): Testable[CollectionCondition[Cond], Iterable[V]] with
    extension (self: CollectionCondition[Cond])
      override def test(v: Iterable[V]): Boolean = self match
        case CollectionCondition.Any(cond)   => v.exists(cond.test)
        case CollectionCondition.Every(cond) => v.forall(cond.test)

enum InventorySlot derives Decoder:
  case Head
  case Body
  case Legs
  case Feet
  case MainHand
  case OffHand
  case SlotIdx(idx: Int)

enum InventoryCondition derives Decoder:
  case CheckAll(cond: BoolCondition[CollectionCondition[ItemCondition]])
  case Slot(slot: InventorySlot, cond: BoolCondition[ItemCondition])

object InventoryCondition:
  given Testable[InventoryCondition, PlayerInventory] with
    extension (self: InventoryCondition)
      override def test(v: PlayerInventory): Boolean = self match
        case InventoryCondition.CheckAll(cond) =>
          cond.test(v.getContents.map(Option(_)))

        case InventoryCondition.Slot(slot, cond) =>
          val item = slot match
            case InventorySlot.Head         => v.getHelmet
            case InventorySlot.Body         => v.getChestplate
            case InventorySlot.Legs         => v.getLeggings
            case InventorySlot.Feet         => v.getBoots
            case InventorySlot.MainHand     => v.getItemInMainHand
            case InventorySlot.OffHand      => v.getItemInOffHand
            case InventorySlot.SlotIdx(idx) => v.getItem(idx)

          cond.test(Option(item))

enum PlayerCondition derives Decoder:
  case ExpLevelProgress(cond: BoolCondition[NumberCondition[Float]])
  case ExpLevel(cond: BoolCondition[NumberCondition[Int]])
  case Health(cond: BoolCondition[NumberCondition[Double]])
  case Inventory(cond: BoolCondition[InventoryCondition])
  case Potion(cond: BoolCondition[CollectionCondition[PotionCondition]])
  case IsSleeping
  case IsDeeplySleeping
  case IsSwimming

object PlayerCondition:
  given Testable[PlayerCondition, Player] with
    extension (self: PlayerCondition)
      def test(v: Player): Boolean = self match
        case PlayerCondition.ExpLevelProgress(cond) => cond.test(v.getExp)
        case PlayerCondition.ExpLevel(cond)         => cond.test(v.getLevel)
        case PlayerCondition.Health(cond)           => cond.test(v.getHealth)
        case PlayerCondition.Inventory(cond)        => cond.test(v.getInventory)
        case PlayerCondition.Potion(cond) =>
          val effects: Iterable[PotionEffect] = v.getActivePotionEffects.asScala // Compiler is throwing a tantrum
          cond.test(effects)
        case IsSleeping       => v.getPlayer.isSleeping
        case IsDeeplySleeping => v.getPlayer.isDeeplySleeping
        case IsSwimming       => v.getPlayer.isSwimming

enum CmdCallbackTrigger derives Decoder:
  case PlayerStopUsingItemEvent
  case PlayerBedLeaveEvent
  case PlayerToggleSneakEvent
  case PlayerJumpEvent
  case PlayerQuitEvent
  case PlayerRespawnEvent
  case PlayerBedFailEnterEvent
  case PlayerToggleSprintEvent
  case PlayerLevelChangeEvent
  case PlayerBedEnterEvent
  case PlayerJoinEvent
  case PlayerExpChangeEvent
  case PlayerGameModeChangeEvent
  case PlayerDeepSleepEvent

case class CmdCallback(
    trigger: CmdCallbackTrigger,
    cmd: String,
    delay: Option[Int],
    conditions: BoolCondition[PlayerCondition]
) derives Decoder
