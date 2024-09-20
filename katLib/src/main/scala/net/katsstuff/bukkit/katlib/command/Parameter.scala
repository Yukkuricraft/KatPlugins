package net.katsstuff.bukkit.katlib.command

import java.util
import java.util.Locale

import scala.annotation.targetName
import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.reflect.ClassTag

import cats.Eval
import cats.arrow.FunctionK
import cats.data.{EitherT, NonEmptyList as NEL, State, StateT}
import cats.syntax.all.*
import net.katsstuff.bukkit.katlib.util.FutureOrNow
import org.bukkit.command.CommandSender

//noinspection ScalaDeprecation
trait Parameter[A]:

  def parse(source: CommandSender)(using ExecutionContext): EitherT[ParameterState, CommandFailure, A]

  def suggestions(source: CommandSender)(using ExecutionContext): ParameterState[List[String]]

  def usage(source: CommandSender): Usage
end Parameter

trait NameableParameter[A] extends Parameter[A]:
  def named(name: String): NameableParameter[A]

trait SyncParameter[A] extends Parameter[A]:
  def parseSync(source: CommandSender): EitherT[SyncParameterState, CommandFailure, A]

  def suggestionsSync(source: CommandSender): SyncParameterState[List[String]]

  override def parse(source: CommandSender)(using ExecutionContext): EitherT[ParameterState, CommandFailure, A] =
    parseSync(source).mapK(new FunctionK[SyncParameterState, ParameterState] {
      override def apply[B](fa: SyncParameterState[B]): ParameterState[B] = fa.mapK(new FunctionK[Eval, FutureOrNow] {
        override def apply[C](fa: Eval[C]): FutureOrNow[C] = FutureOrNow.fromEval(fa)
      })
    })

  override def suggestions(source: CommandSender)(using ExecutionContext): ParameterState[List[String]] =
    suggestionsSync(source).mapK(new FunctionK[Eval, FutureOrNow] {
      override def apply[B](fa: Eval[B]): FutureOrNow[B] = FutureOrNow.fromEval(fa)
    })

type SyncParameterState[A] = State[List[RawCmdArg], A]
type ParameterState[A]     = StateT[FutureOrNow, List[RawCmdArg], A]
type CommandResult[A]      = Either[CommandFailure, A]

infix type ~[A, B] = (A, B)

//noinspection ScalaFileName
@targetName("tupleSyntax")
object `~`:
  def unapply[H, T](t: (H, T)): (H, T) = t

extension (param: Parameter[String])
  @targetName("or") def |(literal: String): Parameter[String] =
    new Parameter.EitherParameter[String, String](param, Parameter.literal(literal)).map(_.merge)

extension (literal: String)
  def named(newName: String): Parameter[String] = new Parameter.AliasParameter(Parameter.literal(literal), newName)

  @targetName("tupled") def ~[B](other: Parameter[B]): Parameter[(String, B)] = Parameter.literal(literal) ~ other
  @targetName("tupled") def ~(otherLiteral: String): Parameter[(String, String)] =
    Parameter.literal(literal) ~ Parameter.literal(otherLiteral)

  @targetName("useRight") def ~>[B](other: Parameter[B]): Parameter[B] = Parameter.literal(literal) ~> other
  @targetName("useRight") def ~>(otherLiteral: String): Parameter[String] =
    Parameter.literal(literal) ~> Parameter.literal(otherLiteral)

  @targetName("useLeft") def <~[B](other: Parameter[B]): Parameter[String] = Parameter.literal(literal) <~ other
  @targetName("useLeft") def <~(otherLiteral: String): Parameter[String] =
    Parameter.literal(literal) <~ Parameter.literal(otherLiteral)

  @targetName("orElse") def ||[B](other: Parameter[B]): Parameter[Either[String, B]] =
    Parameter.literal(literal) || other
  @targetName("orElse") def ||(otherLiteral: String): Parameter[Either[String, String]] =
    Parameter.literal(literal) || Parameter.literal(otherLiteral)

  @targetName("or") def |(other: Parameter[String]): Parameter[String] =
    new Parameter.EitherParameter[String, String](Parameter.literal(literal), other).map(_.merge)
  @targetName("or") def |(otherLiteral: String): Parameter[String] =
    Parameter.literal(NEL.of(literal, otherLiteral))

  def asParameter: Parameter[String] = Parameter.literal(literal)
end extension

def optional[A](parameter: Parameter[A]): Parameter[Option[A]] = Parameter.optional(parameter)

def single[A](parameter: Parameter[Set[A]]): Parameter[A] =
  parameter.emap { set =>
    set.size match
      case 0 => Left(CommandError("Not enough arguments matched"))
      case 1 => Right(set.head)
      case _ => Left(CommandError(s"Too many values matched"))
  }

//noinspection ScalaUnusedSymbol
object Parameter:

  extension [A](param: Parameter[A])
    def named(newName: String): Parameter[A] = param match
      case nameable: NameableParameter[A] => nameable.named(newName)
      case _                              => new Parameter.AliasParameter(param, newName)

    @targetName("tupled") def ~[B](other: Parameter[B]): Parameter[(A, B)] =
      new Parameter.TupleParameter[A, B](param, other)
    @targetName("tupled") def ~(literal: String): Parameter[(A, String)] = param ~ Parameter.literal(literal)

    @targetName("useRight") def ~>[B](other: Parameter[B]): Parameter[B] = (param ~ other).map(_._2)
    @targetName("useRight") def ~>(literal: String): Parameter[String]   = param ~> Parameter.literal(literal)

    @targetName("useLeft") def <~[B](other: Parameter[B]): Parameter[A] = (param ~ other).map(_._1)
    @targetName("useLeft") def <~(literal: String): Parameter[A]        = param <~ Parameter.literal(literal)

    @targetName("orElse") def ||[B](other: Parameter[B]): Parameter[Either[A, B]] =
      new Parameter.EitherParameter[A, B](param, other)
    @targetName("orElse") def ||(literal: String): Parameter[Either[A, String]] = param || Parameter.literal(literal)

    @targetName("or") def |(other: Parameter[A]): Parameter[A] =
      new Parameter.EitherParameter[A, A](param, other).map(_.merge)

    def map[B](f: A => B): Parameter[B] = param.emap(a => Right(f(a)))

    def emap[B](f: A => CommandResult[B]): Parameter[B] = new Parameter.MappedSyncParameter(param, f)

    def amap[B](f: A => FutureOrNow[B])(using ExecutionContext): Parameter[B] =
      new MappedParameter(param, f.andThen(_.map(Right.apply)))

    def aemap[B](f: A => FutureOrNow[CommandResult[B]]): Parameter[B] = new MappedParameter(param, f)

  extension [A](param: Parameter[Option[A]]) def defaultsTo(default: A): Parameter[A] = param.map(_.getOrElse(default))

  class ProxyParameter[A](param: Parameter[A]) extends Parameter[A] {
    override def usage(source: CommandSender): Usage = param.usage(source)

    override def parse(source: CommandSender)(
        using ExecutionContext
    ): EitherT[ParameterState, CommandFailure, A] = param.parse(source)

    override def suggestions(source: CommandSender)(
        using ExecutionContext
    ): ParameterState[List[String]] = param.suggestions(source)
  }

  def singleSyncParameter[A: ClassTag](
      name: String,
      parser: RawCmdArg => CommandResult[A],
      suggestions: (String, CommandSender) => List[String] = (_, _) => Nil
  ): Parameter[A] =
    new SingleSyncParameter(name, parser, suggestions)

  def singleParameter[A: ClassTag](
      name: String,
      parser: RawCmdArg => FutureOrNow[CommandResult[A]],
      suggestions: (String, CommandSender) => FutureOrNow[List[String]] = (_, _) => FutureOrNow.now(Nil)
  ): Parameter[A] =
    new SingleParameter(name, parser, suggestions)

  class SingleSyncParameter[A: ClassTag](
      val name: String,
      parser: RawCmdArg => CommandResult[A],
      suggestionsFunc: (String, CommandSender) => List[String]
  ) extends ProxyParameter[A](
        new SingleParameter(
          name,
          arg => FutureOrNow.now(parser(arg)),
          (s, c) => FutureOrNow.now(suggestionsFunc(s, c))
        )
      )

  class SingleParameter[A: ClassTag](
      val name: String,
      parser: RawCmdArg => FutureOrNow[CommandResult[A]],
      suggestionsFunc: (String, CommandSender) => FutureOrNow[List[String]]
  ) extends Parameter[A],
        NameableParameter[A]:

    override def parse(source: CommandSender)(
        using ExecutionContext
    ): EitherT[ParameterState, CommandFailure, A] =
      EitherT(StateT {
        case Nil                  => FutureOrNow.now((Nil, Left(NotEnoughArguments(name))))
        case x :: xs if x.isEmpty => FutureOrNow.now((xs, Left(NotEnoughArguments(name))))
        case x :: xs =>
          parser(x).map(res => if res.isRight then (xs, res) else (x :: xs, res))
      })

    override def suggestions(source: CommandSender)(using ExecutionContext): ParameterState[List[String]] =
      StateT(s => suggestionsFunc(s.headOption.fold("")(_.content), source).map(s.drop(1) -> _))

    override def usage(source: CommandSender): Usage = Usage.Required(name)

    override def named(name: String): NameableParameter[A] =
      new SingleParameter(name, parser, suggestionsFunc)
  end SingleParameter

  class TupleParameter[A, B](val first: Parameter[A], val second: Parameter[B]) extends Parameter[(A, B)]:
    override def parse(source: CommandSender)(
        using ExecutionContext
    ): EitherT[ParameterState, CommandFailure, (A, B)] =
      first.parse(source).flatMap(a => second.parse(source).map((a, _)))

    override def suggestions(source: CommandSender)(using ExecutionContext): ParameterState[List[String]] =
      StateT { (s: List[RawCmdArg]) =>
        val firstParse = first.parse(source).value.run(s)
        firstParse.flatMap {
          case (_, Left(_))    => first.suggestions(source).run(s)
          case (Nil, Right(_)) => first.suggestions(source).run(s)
          case (s2, Right(_))  => second.suggestions(source).run(s2)
        }
      }

    override def usage(source: CommandSender): Usage =
      Usage.Seq(first.usage(source), NEL.one(second.usage(source))).flatten
  end TupleParameter

  class EitherParameter[A, B](val left: Parameter[A], val right: Parameter[B]) extends Parameter[Either[A, B]]:

    private val leftMapped  = left.map(Left(_): Either[A, B])
    private val rightMapped = right.map(Right(_): Either[A, B])

    override def parse(
        source: CommandSender
    )(using ExecutionContext): EitherT[ParameterState, CommandFailure, Either[A, B]] =
      EitherT(StateT { (s: List[RawCmdArg]) =>
        leftMapped.parse(source).value.run(s).flatMap {
          case (`s`, Left(_)) => rightMapped.parse(source).value.run(s)
          case t              => FutureOrNow.now(t)
        }
      })

    override def suggestions(source: CommandSender)(using ExecutionContext): ParameterState[List[String]] =
      StateT { (s: List[RawCmdArg]) =>
        left.parse(source).value.runS(s).flatMap {
          case `s` =>
            right.parse(source).value.runS(s).flatMap {
              case `s` =>
                (left.suggestions(source).runA(s), right.suggestions(source).runA(s)).tupled
                  .map(_ ++ _)
                  .map((s, _))
              case _ => right.suggestions(source).run(s)
            }
          case _ => left.suggestions(source).run(s)
        }
      }

    override def usage(source: CommandSender): Usage =
      Usage.Choice(List(left.usage(source), right.usage(source))).flatten
  end EitherParameter

  def literal(ss: NEL[String]): Parameter[String] = new LiteralChoicesParameter(ss)
  def literal(literal: String): Parameter[String] = Parameter.literal(NEL.one(literal))

  class LiteralChoicesParameter(ss: NEL[String]) extends Parameter[String]:
    private val ssLowercase = ss.map(_.toLowerCase(Locale.ROOT)).toNes

    override def parse(
        source: CommandSender
    )(using ExecutionContext): EitherT[ParameterState, CommandFailure, String] =
      EitherT(StateT {
        case Nil                  => FutureOrNow.now((Nil, Left(NotEnoughArguments("choice"))))
        case x :: xs if x.isEmpty => FutureOrNow.now((xs, Left(NotEnoughArguments("choice"))))
        case x :: xs =>
          FutureOrNow.now(
            if ssLowercase(x.content.toLowerCase(Locale.ROOT)) then (xs, Right(x.content))
            else (x :: xs, Left(LiteralNotMatched(x.content, ss)))
          )
      })

    override def suggestions(source: CommandSender)(using ExecutionContext): ParameterState[List[String]] =
      StateT {
        case Nil                  => FutureOrNow.now((Nil, Nil))
        case x :: xs if x.isEmpty => FutureOrNow.now((xs, ss.toList))
        case x :: xs =>
          val lowercaseContent = x.content.toLowerCase(Locale.ROOT)
          val matches          = ss.filter(_.regionMatches(false, 0, lowercaseContent, 0, lowercaseContent.length))
          FutureOrNow.now((xs, matches))
      }

    override def usage(source: CommandSender): Usage =
      if ss.tail.isEmpty then Usage.Const(ss.head)
      else Usage.Choice(ss.map(Usage.Const.apply).toList)
  end LiteralChoicesParameter

  def optional[A](p: Parameter[A]): Parameter[Option[A]] = new OptionalParameter[A](p)

  class OptionalParameter[A](val p: Parameter[A]) extends Parameter[Option[A]]:
    override def parse(
        source: CommandSender
    )(using ExecutionContext): EitherT[ParameterState, CommandFailure, Option[A]] =
      p.parse(source).map(Some.apply).recover(_ => None)

    override def suggestions(source: CommandSender)(using ExecutionContext): ParameterState[List[String]] =
      StateT { (s: List[RawCmdArg]) =>
        p.parse(source).value.run(s).flatMap {
          case (`s`, _) => FutureOrNow.now((s, Nil))
          case (s2, _)  => p.suggestions(source).run(s2)
        }
      }

    override def usage(source: CommandSender): Usage =
      Usage.optional(p.usage(source)).flatten
  end OptionalParameter

  class AliasParameter[A](val p: Parameter[A], val name: String) extends ProxyParameter[A](p):
    override def usage(source: CommandSender): Usage = p.usage(source) match {
      case Usage.Choice(List(Usage.Required(_)), true) => Usage.Choice(List(Usage.Required(name)), true)
      case _                                           => Usage.Required(name)
    }
  end AliasParameter

  def choicesSingleOpt[A: ClassTag](
      name: String,
      getValue: String => Option[A],
      suggestionsChoices: => Seq[String] = Nil,
      showChoicesInUsage: Boolean = false,
      showSuggestions: Boolean = true
  ): Parameter[A] =
    new ChoicesSingleSyncParameter[A](
      name,
      showChoicesInUsage,
      showSuggestions,
      (_, s) => getValue(s),
      _ => suggestionsChoices,
    )

  def choicesSingleMap[A: ClassTag](
      name: String,
      generateChoices: => Map[String, A],
      showChoicesInUsage: Boolean = false,
      showSuggestions: Boolean = true,
      caseSensitive: Boolean = false
  ): Parameter[A] =
    new ChoicesSingleSyncParameter[A](
      name,
      showChoicesInUsage,
      showSuggestions,
      _ => generateChoices,
      caseSensitive,
    )

  def choices[A](
      name: String,
      generateChoices: => Map[String, A],
      showChoicesInUsage: Boolean = false,
      showSuggestions: Boolean = true,
      caseSensitive: Boolean = false
  ): Parameter[Set[A]] =
    new ChoicesManySyncParameter[A](
      name,
      showChoicesInUsage,
      showSuggestions,
      _ => generateChoices,
      caseSensitive,
    )

  def choicesSingleOptAsync[A: ClassTag](
      name: String,
      getValue: String => FutureOrNow[Option[A]],
      suggestionsChoices: => FutureOrNow[Seq[String]] = FutureOrNow.now(Nil),
      showChoicesInUsage: Boolean = false,
      showSuggestions: Boolean = true
  ): Parameter[A] =
    new ChoicesSingleParameter[A](
      name,
      showChoicesInUsage,
      showSuggestions,
      (_, s) => getValue(s),
      _ => suggestionsChoices,
    )

  def choicesSingleMapAsync[A: ClassTag](
      name: String,
      generateChoices: => FutureOrNow[Map[String, A]],
      showChoicesInUsage: Boolean = false,
      showSuggestions: Boolean = true,
      caseSensitive: Boolean = false
  )(using ExecutionContext): Parameter[A] =
    new ChoicesSingleParameter[A](
      name,
      showChoicesInUsage,
      showSuggestions,
      _ => generateChoices,
      caseSensitive,
    )

  def choicesAsync[A](
      name: String,
      generateChoices: => FutureOrNow[Map[String, A]],
      showChoicesInUsage: Boolean = false,
      showSuggestions: Boolean = true,
      caseSensitive: Boolean = false
  ): Parameter[Set[A]] =
    new ChoicesManyParameter[A](
      name,
      showChoicesInUsage,
      showSuggestions,
      _ => generateChoices,
      caseSensitive,
    )

  private val existingSorted: mutable.AnyRefMap[AnyRef, Array[String]] = mutable.AnyRefMap()

  private def getSorted[A <: AnyRef](
      computedFromFut: FutureOrNow[A],
      choicesF: A => Iterable[String]
  )(using ExecutionContext): FutureOrNow[Array[String]] =
    computedFromFut.map { computedFrom =>
      existingSorted.get(computedFrom) match
        case Some(value) => value
        case None =>
          val choices = choicesF(computedFrom)
          existingSorted.getOrElseUpdate(
            computedFrom, {
              val arr = choices.toArray
              util.Arrays.sort(arr, String.CASE_INSENSITIVE_ORDER)
              arr
            }
          )
    }

  private def choiceSuggestions[A <: AnyRef](
      computedFrom: => FutureOrNow[A],
      choices: A => Iterable[String]
  )(using ExecutionContext): ParameterState[List[String]] =
    StateT
      .inspectF[FutureOrNow, List[RawCmdArg], List[String]] {
        case Nil                 => FutureOrNow.now(Nil)
        case x :: _ if x.isEmpty => getSorted(computedFrom, choices).map(_.iterator.take(30).toList)
        case x :: _ =>
          val content = x.content
          getSorted(computedFrom, choices).map { allArr =>
            // This code tries to grab elements from the array that are roughly close to the input being written

            // Start of where to look. If the result is negative, we need to invert it and subtract one to get the
            // index where the content would be inserted
            val maybeNegFromIdx = util.Arrays.binarySearch(allArr, content, String.CASE_INSENSITIVE_ORDER)
            val fromIdx         = if maybeNegFromIdx < 0 then (maybeNegFromIdx * -1) - 1 else maybeNegFromIdx

            // We alter the content, replacing the last letter with the next one
            // We then search for this string to get an index beyond which there can be no match
            // Like before we need to fix the value if it is negative
            val nextElement =
              content.substring(0, content.length - 1) + (content.charAt(content.length - 1) + 1).toChar
            val maybeNegToIdx =
              util.Arrays.binarySearch(
                allArr,
                (fromIdx + 1).min(allArr.length),
                allArr.length,
                nextElement,
                String.CASE_INSENSITIVE_ORDER
              )
            val toIdx = if maybeNegToIdx < 0 then (maybeNegToIdx * -1) - 1 else maybeNegToIdx

            allArr.iterator
              .slice(fromIdx, toIdx.min(fromIdx + 30))
              .filter(_.regionMatches(true, 0, content, 0, content.length))
              .toList
          }
      }
      .modify(_.drop(1))

  class ChoicesSingleSyncParameter[A: ClassTag](
      val name: String,
      showChoicesInUsage: Boolean = false,
      showSuggestions: Boolean = true,
      getValue: (CommandSender, String) => Option[A],
      suggestionsChoices: CommandSender => Seq[String],
  ) extends ProxyParameter[A](
        new ChoicesSingleParameter[A](
          name,
          showChoicesInUsage,
          showSuggestions,
          (c, s) => FutureOrNow.now(getValue(c, s)),
          c => FutureOrNow.now(suggestionsChoices(c)),
        )
      ):

    def this(
        name: String,
        showChoicesInUsage: Boolean,
        showSuggestions: Boolean,
        choices: CommandSender => Map[String, A],
        caseSensitive: Boolean,
    ) =
      this(
        name,
        showChoicesInUsage,
        showSuggestions,
        (c, s) => {
          val generatedChoices = choices(c)
          if caseSensitive then generatedChoices.get(s)
          else
            generatedChoices
              .get(s)
              .orElse(generatedChoices.map(t => t._1.toLowerCase(Locale.ROOT) -> t._2).get(s.toLowerCase(Locale.ROOT)))
        },
        choices(_).keys.toSeq,
      )
  end ChoicesSingleSyncParameter

  class ChoicesSingleParameter[A: ClassTag](
      val name: String,
      showChoicesInUsage: Boolean = false,
      showSuggestions: Boolean = true,
      getValue: (CommandSender, String) => FutureOrNow[Option[A]],
      suggestionsChoices: CommandSender => FutureOrNow[Seq[String]],
  ) extends Parameter[A],
        NameableParameter[A]:

    def this(
        name: String,
        showChoicesInUsage: Boolean,
        showSuggestions: Boolean,
        choices: CommandSender => FutureOrNow[Map[String, A]],
        caseSensitive: Boolean,
    )(using ExecutionContext) =
      this(
        name,
        showChoicesInUsage,
        showSuggestions,
        (c, s) =>
          choices(c).map { generatedChoices =>
            if caseSensitive then generatedChoices.get(s)
            else
              generatedChoices
                .get(s)
                .orElse(
                  generatedChoices.map(t => t._1.toLowerCase(Locale.ROOT) -> t._2).get(s.toLowerCase(Locale.ROOT))
                )
          },
        choices(_).map(_.keys.toSeq),
      )

    override def parse(source: CommandSender)(using ExecutionContext): EitherT[ParameterState, CommandFailure, A] =
      EitherT(StateT {
        case Nil                  => FutureOrNow.now((Nil, Left(NotEnoughArguments(name))))
        case x :: xs if x.isEmpty => FutureOrNow.now((xs, Left(NotEnoughArguments(name))))
        case x :: xs =>
          val head = x.content

          getValue(source, head).map { value =>
            val res = value.toRight(CommandSyntaxError(s"$head is not a valid $name", x.start))
            if res.isRight
            then (xs, res)
            else (x :: xs, res)
          }
      })

    override def suggestions(source: CommandSender)(using ExecutionContext): ParameterState[List[String]] =
      if showSuggestions
      then choiceSuggestions(suggestionsChoices(source), identity)
      else StateT.modify[FutureOrNow, List[RawCmdArg]](_.drop(1)).map(_ => Nil)

    override def usage(source: CommandSender): Usage =
      lazy val current = suggestionsChoices(source).value.toOption.toSeq.flatMap(_.value)

      if current.length > 10 || !showChoicesInUsage then Usage.Required(name)
      else Usage.Choice(current.map(Usage.Const.apply).toList)

    override def named(name: String): NameableParameter[A] =
      new ChoicesSingleParameter(name, showSuggestions, showSuggestions, getValue, suggestionsChoices)
  end ChoicesSingleParameter

  class ChoicesManySyncParameter[A](
      val name: String,
      showChoicesInUsage: Boolean = false,
      showSuggestions: Boolean = true,
      choices: CommandSender => Map[String, A],
      caseSensitive: Boolean = false,
  ) extends ProxyParameter[Set[A]](
        new ChoicesManyParameter(
          name,
          showChoicesInUsage,
          showSuggestions,
          c => FutureOrNow.now(choices(c)),
          caseSensitive,
        )
      )

  class ChoicesManyParameter[A](
      val name: String,
      showChoicesInUsage: Boolean = false,
      showSuggestions: Boolean = true,
      choices: CommandSender => FutureOrNow[Map[String, A]],
      caseSensitive: Boolean = false,
  ) extends Parameter[Set[A]],
        NameableParameter[Set[A]]:

    override def parse(source: CommandSender)(
        using ExecutionContext
    ): EitherT[ParameterState, CommandFailure, Set[A]] =
      EitherT(StateT {
        case Nil                  => FutureOrNow.now((Nil, Left(NotEnoughArguments(name))))
        case x :: xs if x.isEmpty => FutureOrNow.now((xs, Left(NotEnoughArguments(name))))
        case x :: xs =>
          choices(source).map { resolvedChoices =>
            val head = x.content

            lazy val matchingRegions = resolvedChoices.collect {
              case (k, v) if k.regionMatches(!caseSensitive, 0, head, 0, head.length) => v
            }.toSet

            if resolvedChoices.contains(head) then (xs, Right(Set(resolvedChoices(head))))
            else if matchingRegions.nonEmpty then (xs, Right(matchingRegions))
            else (x :: xs, Left(CommandSyntaxError(s"$head is not a valid $name", x.start)))
          }
      })

    override def suggestions(source: CommandSender)(using ExecutionContext): ParameterState[List[String]] =
      if showSuggestions
      then choiceSuggestions(choices(source), _.keys)
      else StateT.modify[FutureOrNow, List[RawCmdArg]](_.drop(1)).map(_ => Nil)

    override def usage(source: CommandSender): Usage =
      lazy val current = choices(source).value.toOption.toSeq.flatMap(_.value.keys)

      if !showChoicesInUsage || current.size > 10 then Usage.Required(name)
      else Usage.Choice(current.map(Usage.Const.apply).toList)

    override def named(name: String): NameableParameter[Set[A]] =
      new ChoicesManyParameter(name, showChoicesInUsage, showSuggestions, choices, caseSensitive)
  end ChoicesManyParameter

  class MappedSyncParameter[A, B](val p: Parameter[A], f: A => CommandResult[B])
      extends ProxyParameter[B](
        new MappedParameter[A, B](p, f.andThen(FutureOrNow.now))
      )

  // noinspection ScalaDeprecation
  class MappedParameter[A, B](val p: Parameter[A], f: A => FutureOrNow[CommandResult[B]]) extends Parameter[B]:

    override def parse(source: CommandSender)(using ExecutionContext): EitherT[ParameterState, CommandFailure, B] =
      EitherT(p.parse(source).value.flatMapF(e => e.fold(l => FutureOrNow.now(Left(l)), r => f(r))))

    override def suggestions(source: CommandSender)(using ExecutionContext): ParameterState[List[String]] =
      p.suggestions(source)

    override def usage(source: CommandSender): Usage = p.usage(source)
  end MappedParameter

  protected[katlib] class CachedParameter[A](
      parameter: Parameter[A],
      parseF: () => FutureOrNow[(List[RawCmdArg], Either[CommandFailure, A])],
      suggestionsF: () => FutureOrNow[(List[RawCmdArg], List[String])]
  ) extends ProxyParameter(parameter):

    private lazy val parseRes       = parseF().memoize
    private lazy val suggestionsRes = suggestionsF().memoize

    override def parse(source: CommandSender)(using ExecutionContext): EitherT[ParameterState, CommandFailure, A] =
      EitherT(StateT(_ => parseRes))

    override def suggestions(source: CommandSender)(using ExecutionContext): ParameterState[List[String]] =
      StateT(_ => suggestionsRes)
