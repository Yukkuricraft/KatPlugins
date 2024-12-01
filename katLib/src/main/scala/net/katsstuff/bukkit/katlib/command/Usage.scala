package net.katsstuff.bukkit.katlib.command

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.ClassTag

import cats.data.{NonEmptyList, NonEmptyList as NEL}

enum Usage(val isSimple: Boolean):
  case Required(s: String)                                      extends Usage(true)
  case Const(s: String)                                         extends Usage(true)
  case Seq(head: Usage, tail: NEL[Usage])                       extends Usage(false)
  case Choice(choices: List[Usage], allowNone: Boolean = false) extends Usage(false)

  def flatten: Usage = this match {
    case Seq(head, tail) =>
      val newElems = NEL(head, tail.toList).map(_.flatten).flatMap {
        case Seq(innerHead, innerTail) => NEL(innerHead, innerTail.toList)
        case u                         => NEL.one(u)
      }

      newElems.length match
        case 0 => Choice(Nil, true)
        case 1 => newElems.head
        case _ => Seq(newElems.head, NEL.fromListUnsafe(newElems.tail))
    case Choice(choices, allowNone) =>
      val flattenedChoices = choices.map(_.flatten)

      val newChoices = flattenedChoices.flatMap {
        case Choice(innerChoices, _) => innerChoices
        case u                       => scala.Seq(u)
      }

      val newAllowNone = allowNone || flattenedChoices.exists {
        case Choice(_, allowNone) => allowNone
        case _                    => false
      }

      newChoices.length match
        case 0 => Choice(Nil, true)
        case 1 => if newAllowNone then Choice(List(newChoices.head), true) else newChoices.head
        case _ => Choice(newChoices, newAllowNone)
    case _ => this
  }

  def printUsage: String =
    def printHeadNested[A](u: Usage)(using tag: ClassTag[A]): String =
      if u.isSimple || tag.runtimeClass.isInstance(u) then innerPrintUsage(u)
      else s"(${innerPrintUsage(u)})"

    def innerPrintUsage(usage: Usage): String = usage.flatten match
      case Choice(scala.Seq(Required(s)), true) => s"[$s]"
      case Required(s)                          => s"<$s>"
      case Const(s)                             => s
      case Seq(head, tail)        => (head :: tail).map(innerPrintUsage).filter(_.nonEmpty).toList.mkString(" ")
      case Choice(Nil, true)      => ""
      case Choice(choices, true)  => s"[${choices.map(innerPrintUsage).filter(_.nonEmpty).mkString(" | ")}]"
      case Choice(choices, false) => s"(${choices.map(innerPrintUsage).filter(_.nonEmpty).mkString(" | ")})"

    val res = innerPrintUsage(this)
    if res.startsWith("(") && res.endsWith(")") then res.substring(1, res.length - 1) else res
  end printUsage

object Usage:

  def optional(u: Usage): Usage = Choice(List(u), allowNone = true)

  val Empty: Usage = Choice(Nil, allowNone = true)

  private def mergeChoiceSelf(choice: Choice, startAtIdx: Int): Usage =
    def finishLoop(newChoices: List[Usage]): Usage =
      if newChoices.length == 1 then if choice.allowNone then optional(newChoices.head) else newChoices.head
      else Choice(newChoices, choice.allowNone)

    @tailrec
    def loop(remaining: List[Usage], acc: List[Usage]): Usage = remaining match
      case fst :: snd :: rest =>
        merge(fst, snd) match
          case Choice(List(`fst`, `snd`), false) =>
            val newAcc = snd :: fst :: acc
            finishLoop(newAcc.reverse)
          case merged =>
            loop(merged :: rest, acc)
      case rest =>
        finishLoop(acc reverse_::: rest)

    val (accReversed, input) = choice.choices.splitAt(startAtIdx)

    loop(input, accReversed.reverse)

  def merge(first: Usage, second: Usage): Usage = (first.flatten, second.flatten) match
    case (Required(s1), Required(s2)) if s1 == s2 => Required(s1)
    case (Const(s1), Const(s2)) if s1 == s2       => Const(s1)
    case (Seq(firstHead, firstTail), Seq(secondHead, secondTail)) =>
      @tailrec
      def loop(first: List[Usage], second: List[Usage], acc: List[Usage]): Usage = (first, second) match
        case ((choice @ Choice(_, _)) :: Nil, ys) =>
          val res =
            NEL
              .fromList(ys)
              .map(nel => NEL.fromList(nel.tail).fold(nel.head)(Seq(nel.head, _)))
              .fold(Choice(choice.choices, allowNone = true))(merge(choice, _))

          loop(Nil, Nil, res :: acc)
        case (xs, (choice @ Choice(_, _)) :: Nil) =>
          val res =
            NEL
              .fromList(xs)
              .map(nel => NEL.fromList(nel.tail).fold(nel.head)(Seq(nel.head, _)))
              .fold(Choice(choice.choices, allowNone = true))(merge(choice, _))

          loop(Nil, Nil, res :: acc)

        case (x :: xs, y :: ys) =>
          merge(x, y) match
            case Choice(_, _) if xs.headOption != ys.headOption || xs.length != ys.length =>
              val firstU  = NEL.fromList(xs).fold(x)(Seq(x, _))
              val secondU = NEL.fromList(ys).fold(y)(Seq(y, _))
              val choice  = Choice(List(firstU, secondU))

              NEL.fromList(acc.reverse).fold(choice)(nel => Seq(nel.head, NEL.ofInitLast(nel.tail, choice)))
            case merged => loop(xs, ys, merged :: acc)

        case (Nil, Nil) =>
          acc.reverse match
            case x :: y :: rest => Seq(x, NEL(y, rest))
            case x :: Nil       => x
            case Nil            => Choice(Nil, allowNone = true)

        case (Nil, y :: ys) =>
          val tail = optional(NEL.fromList(ys).fold(y)(Seq(y, _)))

          acc.reverse match
            case h :: t => Seq(h, NEL.ofInitLast(t, tail))
            case Nil    => tail

        case (x :: xs, Nil) =>
          val tail = optional(NEL.fromList(xs).fold(x)(Seq(x, _)))

          acc.reverse match
            case h :: t => Seq(h, NEL.ofInitLast(t, tail))
            case Nil    => tail

      loop((firstHead :: firstTail).toList, (secondHead :: secondTail).toList, Nil)

    case (head, Seq(seqHead, NEL(restHead, restOther))) if head == seqHead =>
      Seq(head, NEL.one(optional(NEL.fromList(restOther).fold(restHead)(Seq(restHead, _)))))

    case (Choice(firstChoices, allowNoneFirst), Choice(secondChoices, allowNoneSecond)) =>
      mergeChoiceSelf(
        Choice(
          (firstChoices ++ secondChoices).distinct,
          allowNoneFirst || allowNoneSecond
        ),
        firstChoices.length - 1
      )
    case (Choice(choices, allowNone), u2) => mergeChoiceSelf(Choice(choices :+ u2, allowNone), choices.length - 1)
    case (u1, Choice(choices, allowNone)) => mergeChoiceSelf(Choice(choices :+ u1, allowNone), choices.length - 1)
    case (u1, u2)                         => Choice(List(u1, u2))
  end merge
end Usage
