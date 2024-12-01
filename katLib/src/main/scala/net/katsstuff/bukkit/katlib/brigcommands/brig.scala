package net.katsstuff.bukkit.katlib.brigcommands

import java.util.function.Predicate

import scala.annotation.targetName
import scala.concurrent.ExecutionContext
import scala.jdk.CollectionConverters.*
import scala.reflect.ClassTag
import scala.util.boundary
import scala.util.boundary.Label

import cats.data.NonEmptyList
import cats.syntax.all.*
import com.mojang.brigadier.arguments.ArgumentType
import com.mojang.brigadier.builder.{ArgumentBuilder, LiteralArgumentBuilder, RequiredArgumentBuilder}
import com.mojang.brigadier.context.CommandContext
import com.mojang.brigadier.suggestion.SuggestionProvider
import com.mojang.brigadier.tree.{CommandNode, LiteralCommandNode}
import com.mojang.brigadier.{Command, SingleRedirectModifier}
import io.papermc.paper.command.brigadier.{CommandSourceStack, Commands}
import net.katsstuff.bukkit.katlib.text.*
import net.katsstuff.bukkit.katlib.util.FutureOrNow
import org.bukkit.command.CommandSender

//noinspection UnstableApiUsage
type Source = CommandSourceStack

//noinspection UnstableApiUsage
case class BrigRootCommand(
    node: LiteralCommandNode[Source],
    description: String,
    aliases: Seq[String],
    usage: CommandInfo
):
  def register(commands: Commands): Set[String] =
    commands.register(node, if description.isEmpty then null else description, aliases.asJava).asScala.toSet

infix type ~[A, B] = (A, B)

// noinspection ScalaFileName
@targetName("tupleSyntax")
object `~`:
  def unapply[H, T](t: (H, T)): (H, T) = t

sealed trait BuilderArgs:
  type Builder <: ArgumentBuilder[Source, Builder]
  def nodeBuilder: Builder
  def usageBuilder: CommandInfo.Builder

object BuilderArgs:
  type Aux[Builder0] = BuilderArgs { type Builder = Builder0 }
  case class Impl[Builder0 <: ArgumentBuilder[Source, Builder0]](
      nodeBuilder: Builder0,
      usageBuilder: CommandInfo.Builder
  ) extends BuilderArgs:
    type Builder = Builder0

object ArgWrapper:
  trait SingleArgLike[A]:
    def name: String
    def argType: ArgumentType[A]

enum ArgWrapper[A]:
  case Single(name: String, argType: ArgumentType[A], clazz: Class[A])
      extends ArgWrapper[A],
      ArgWrapper.SingleArgLike[A]
  case SingleOptional[A2](name: String, argType: ArgumentType[A2], clazz: Class[A2])
      extends ArgWrapper[Option[A2]],
      ArgWrapper.SingleArgLike[A2]
  case None[A2]()                                    extends ArgWrapper[Option[A2]]
  case Zip[B, C](b: ArgWrapper[B], c: ArgWrapper[C]) extends ArgWrapper[(B, C)]
  case Mapped[B, C](from: ArgWrapper[B], f: Label[CommandResult.Error] => (B, CommandContext[Source]) => C)
      extends ArgWrapper[C]

  def map[B](f: Label[CommandResult.Error] ?=> A => B): ArgWrapper[B] =
    Mapped(this, label => (a, _) => f(using label)(a))
  def mapWithCtx[B](f: Label[CommandResult.Error] ?=> (A, CommandContext[Source]) => B): ArgWrapper[B] =
    Mapped(this, label => f(using label))

  @targetName("zip") def ~[B](that: ArgWrapper[B]): Zip[A, B] = Zip(this, that)

  def get(using ctx: CommandContext[Source])(using cmdRes: Label[CommandResult.Error]): A = this match
    case Single(name, _, clazz) => ctx.getArgument(name, clazz)
    case SingleOptional(name, _, clazz) =>
      try Some(ctx.getArgument(name, clazz))
      catch
        case e: IllegalArgumentException =>
          if e.getMessage == s"No such argument '$name' exists on this command" then scala.None
          else throw e
    case None() => scala.None

    case Zip(b, c) =>
      (b.get, c.get)

    case Mapped(from, f) =>
      f(cmdRes)(from.get, ctx)

val noRequirement: Predicate[Source]               = _ => true
val defaultSuggestions: SuggestionProvider[Source] = (_, b) => b.buildFuture()

def permission(permission: String, permissions: String*): Predicate[Source] = source =>
  source.getSender.hasPermission(permission) && permissions.forall(source.getSender.hasPermission)

//noinspection UnstableApiUsage
def rootLiteral(
    literal: String,
    description: String = "",
    aliases: Seq[String] = Nil,
    requirement: Predicate[Source] = noRequirement
)(
    use: BuilderArgs.Aux[LiteralArgumentBuilder[Source]] ?=> Unit
): BrigRootCommand =
  val builder = BuilderArgs.Impl(Commands.literal(literal), CommandInfo.Builder())
  if requirement ne noRequirement then builder.nodeBuilder.requires(requirement)

  use(using builder)
  BrigRootCommand(
    builder.nodeBuilder.build(),
    description,
    aliases,
    CommandInfo.Seq(CommandInfo.Const(literal, requirement), NonEmptyList.one(builder.usageBuilder.build)).flatten
  )

//noinspection UnstableApiUsage
def literal(literal: String, requirement: Predicate[Source] = noRequirement)(
    use: BuilderArgs.Aux[LiteralArgumentBuilder[Source]] ?=> Unit
)(using outerBuilder: BuilderArgs): outerBuilder.type =
  val newBuilder = BuilderArgs.Impl(Commands.literal(literal), CommandInfo.Builder())
  if requirement ne noRequirement then newBuilder.nodeBuilder.requires(requirement)

  use(using newBuilder)
  outerBuilder.nodeBuilder.`then`(newBuilder.nodeBuilder)
  outerBuilder.usageBuilder.addSeq(CommandInfo.Const(literal, requirement), newBuilder.usageBuilder)
  outerBuilder

def literals(literals: Seq[String], requirement: Predicate[Source] = noRequirement)(
    use: BuilderArgs.Aux[LiteralArgumentBuilder[Source]] ?=> String => Unit
)(using outerBuilder: BuilderArgs): outerBuilder.type =
  literals.foreach(l => literal(l, requirement)(use(l)))
  outerBuilder

def literals(literals: String*)(
    use: BuilderArgs.Aux[LiteralArgumentBuilder[Source]] ?=> String => Unit
)(using outerBuilder: BuilderArgs): outerBuilder.type =
  literals.foreach(l => literal(l)(use(l)))
  outerBuilder

private def makeArgBuilder[A](
    arg: ArgWrapper.SingleArgLike[A],
    requirement: Predicate[Source] = noRequirement,
    suggestions: SuggestionProvider[Source] = defaultSuggestions
): BuilderArgs.Aux[RequiredArgumentBuilder[Source, A]] =
  val newBuilder = BuilderArgs.Impl(Commands.argument(arg.name, arg.argType), CommandInfo.Builder())
  if requirement ne noRequirement then newBuilder.nodeBuilder.requires(requirement)
  if suggestions ne defaultSuggestions then newBuilder.nodeBuilder.suggests(suggestions)
  else newBuilder.nodeBuilder.suggests(arg.argType.listSuggestions)

  newBuilder

private def makeArg[A: ClassTag](name: String, arg: ArgumentType[A]): ArgWrapper.Single[A] =
  ArgWrapper.Single(name, arg, summon[ClassTag[A]].runtimeClass.asInstanceOf[Class[A]])

//noinspection UnstableApiUsage
def arg[A: ClassTag](
    name: String,
    arg: ArgumentType[A],
    requirement: Predicate[Source] = noRequirement,
    suggestions: SuggestionProvider[Source] = defaultSuggestions
)(
    use: BuilderArgs.Aux[RequiredArgumentBuilder[Source, A]] ?=> ArgWrapper.Single[A] => Unit
)(using outerBuilder: BuilderArgs): outerBuilder.type =
  val argWrapper = makeArg(name, arg)
  val newBuilder = makeArgBuilder(argWrapper, requirement, suggestions)

  use(using newBuilder)(argWrapper)
  outerBuilder.nodeBuilder.`then`(newBuilder.nodeBuilder)
  outerBuilder.usageBuilder.addSeq(CommandInfo.Required(name, requirement), newBuilder.usageBuilder)
  outerBuilder

private def makeOptArg[A: ClassTag](name: String, arg: ArgumentType[A]): ArgWrapper.SingleOptional[A] =
  ArgWrapper.SingleOptional(name, arg, summon[ClassTag[A]].runtimeClass.asInstanceOf[Class[A]])

//noinspection DuplicatedCode
def optArg[A: ClassTag](
    name: String,
    arg: ArgumentType[A],
    requirement: Predicate[Source] = noRequirement,
    suggestions: SuggestionProvider[Source] = defaultSuggestions
)(
    use: BuilderArgs ?=> ArgWrapper.SingleOptional[A] => Unit
)(using outerBuilder: BuilderArgs): outerBuilder.type =
  val argWrapper = makeOptArg(name, arg)
  val newBuilder = makeArgBuilder(argWrapper, requirement, suggestions)

  use(using newBuilder)(argWrapper)
  use(using outerBuilder)(argWrapper)
  outerBuilder.nodeBuilder.`then`(newBuilder.nodeBuilder)
  outerBuilder.usageBuilder.addSeq(
    CommandInfo.optional(CommandInfo.Required(name, requirement)),
    newBuilder.usageBuilder
  )
  outerBuilder

enum CommandResult[+A]:
  case Success(value: A)
  case Error(message: String) extends CommandResult[Nothing]

def executesRaw(run: Command[Source])(
    using builder: BuilderArgs
): Unit =
  builder.nodeBuilder.executes(run)

// Inline to increase the chance that break gets rewritten to a jump
inline def executes(
    help: CommandSender => Text = _ => Text.Empty,
    description: CommandSender => Text = _ => Text.Empty
)(inline run: Label[CommandResult[Int]] ?=> CommandContext[Source] ?=> Int)(
    using builder: BuilderArgs
): Unit =
  builder.usageBuilder += CommandInfo.Info(help, description)
  executesRaw: ctx =>
    val result = boundary[CommandResult[Int]]: label ?=>
      CommandResult.Success(run(using label)(using ctx))

    result match
      case CommandResult.Success(value) => value
      case CommandResult.Error(message) =>
        ctx.getSource.getSender.sendMessage(t"$Red$message")
        0

inline def asyncExecutes(
    help: CommandSender => Text = _ => Text.Empty,
    description: CommandSender => Text = _ => Text.Empty
)(inline run: Label[CommandResult[Int]] ?=> CommandContext[Source] ?=> FutureOrNow[Either[String, Int]])(
    using builder: BuilderArgs,
    ec: ExecutionContext
): Unit =
  executes(help, description):
    val result = boundary[CommandResult[Int]]: label ?=>
      run(using label).value match
        case Right(eval) => CommandResult.Success(eval.value.getOrError)
        case Left(fut) =>
          fut.foreach:
            case Left(message) =>
              useSender().sendMessage(t"$Red$message")

            case Right(_) =>
          CommandResult.Success(1)

    result match
      case CommandResult.Success(value) => value
      case CommandResult.Error(message) =>
        useSender().sendMessage(t"$Red$message")
        0

def redirect(
    help: CommandSender => Text = _ => Text.Empty,
    description: CommandSender => Text = _ => Text.Empty,
    redirectTo: String = ""
)(to: CommandNode[Source], modifier: SingleRedirectModifier[Source] = null)(using builder: BuilderArgs) =
  builder.usageBuilder += CommandInfo.Info(help, description, redirectTo = Some(redirectTo))
  builder.nodeBuilder.redirect(to, modifier)
