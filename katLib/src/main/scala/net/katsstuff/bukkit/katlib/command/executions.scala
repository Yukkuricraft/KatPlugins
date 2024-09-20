package net.katsstuff.bukkit.katlib.command

import scala.annotation.nowarn
import scala.concurrent.ExecutionContext

import cats.data.{EitherT, NonEmptyList, ValidatedNel}
import cats.syntax.all.*
import net.katsstuff.bukkit.katlib.ScalaPlugin
import net.katsstuff.bukkit.katlib.command.Parameter.CachedParameter
import net.katsstuff.bukkit.katlib.text.*
import net.katsstuff.bukkit.katlib.util.FutureOrNow
import org.bukkit.command.CommandSender

sealed trait Executions:

  def handleCommandAsync(
      source: CommandSender,
      input: List[RawCmdArg]
  )(using ExecutionContext): FutureOrNow[ValidatedNel[(Int, CommandFailure, Usage), Unit]]

  def tabCompleteAsync(source: CommandSender, args: List[RawCmdArg])(using ExecutionContext): FutureOrNow[List[String]]

  def addArgFirst[Arg](parameter: Parameter[Arg]): Executions

  def flatten: Executions

  def children: NonEmptyList[Executions]

  def runExecutions: NonEmptyList[BaseRunExecution[_, _, _]]

end Executions

def execution[Args, Sender](
    args: Parameter[Args] = Parameters.unit,
    sender: UserValidator[Sender] = Senders.commandSender,
    permissions: String = "",
    help: CommandSender => Option[Text] = _ => None,
    description: CommandSender => Option[Text] = _ => None
)(
    f: (Sender, Args) => Either[String, Unit]
)(using ScalaPlugin): Executions = Command.execution(args, sender, permissions, help, description)(f)

def asyncExecution[Args, Sender](
    args: Parameter[Args] = Parameters.unit,
    sender: UserValidator[Sender] = Senders.commandSender,
    permissions: String = "",
    help: CommandSender => Option[Text] = _ => None,
    description: CommandSender => Option[Text] = _ => None
)(
    f: ExecutionContext ?=> (Sender, Args) => FutureOrNow[Either[String, Unit]]
)(using ScalaPlugin): Executions = Command.asyncExecution(args, sender, permissions, help, description)(f)

def withArg[Args](arg: Parameter[Args])(f: Parameter[Args] => NonEmptyList[Executions]): Executions =
  Command.withArg(arg)(f)

def withArgCached[Args](arg: Parameter[Args])(f: Parameter[Args] => NonEmptyList[Executions]): Executions =
  Command.withArgCached(arg)(f)

def subCommand(name: String, names: String*)(child: Executions, children: Executions*): Executions =
  Command.subCommand(name, names: _*)(child, children: _*)

case class AggExecutions(executions: NonEmptyList[Executions]) extends Executions:
  override def handleCommandAsync(
      source: CommandSender,
      input: List[RawCmdArg]
  )(using ExecutionContext): FutureOrNow[ValidatedNel[(Int, CommandFailure, Usage), Unit]] =
    val res = executions.map(e => () => e.handleCommandAsync(source, input))
    res.tail.foldLeft(res.head()) { (acc, run) =>
      acc.flatMap(v => if v.isValid then FutureOrNow.now(v) else run().map(v.findValid))
    }

  override def tabCompleteAsync(source: CommandSender, args: List[RawCmdArg])(
      using ExecutionContext
  ): FutureOrNow[List[String]] =
    FutureOrNow.sequence(executions.toList.map(_.tabCompleteAsync(source, args))).map(_.flatten)

  override def flatten: Executions = AggExecutions(children)

  override def children: NonEmptyList[Executions] = executions.flatMap(_.children)

  override def addArgFirst[Arg](parameter: Parameter[Arg]): Executions =
    AggExecutions(executions.map(_.addArgFirst(parameter)))

  override lazy val runExecutions: NonEmptyList[BaseRunExecution[_, _, _]] = executions.flatMap(_.runExecutions)
end AggExecutions

case class CachedParamExecution[A](
    param: Parameter[A],
    childrenFun: Parameter[A] => NonEmptyList[Executions]
) extends Executions {

  // noinspection ScalaDeprecation
  @nowarn("cat=deprecation")
  private def cachedAggExecutions(source: CommandSender, input: List[RawCmdArg])(
      using ExecutionContext
  ): AggExecutions =
    val parseAsyncF: () => FutureOrNow[(List[RawCmdArg], Either[CommandFailure, A])] = () =>
      param.parse(source).value.run(input)
    val suggestionsAsyncF: () => FutureOrNow[(List[RawCmdArg], List[String])] = () =>
      param.suggestions(source).run(input)

    AggExecutions(childrenFun(new CachedParameter(param, parseAsyncF, suggestionsAsyncF)))

  override def handleCommandAsync(source: CommandSender, input: List[RawCmdArg])(
      using ExecutionContext
  ): FutureOrNow[ValidatedNel[(Int, CommandFailure, Usage), Unit]] =
    cachedAggExecutions(source, input).handleCommandAsync(source, input)

  override def tabCompleteAsync(source: CommandSender, input: List[RawCmdArg])(
      using ExecutionContext
  ): FutureOrNow[List[String]] =
    cachedAggExecutions(source, input).tabCompleteAsync(source, input)

  override def addArgFirst[Arg](parameter: Parameter[Arg]): Executions =
    CachedParamExecution(parameter ~> param, childrenFun)

  override def flatten: Executions = this

  override def children: NonEmptyList[Executions] = childrenFun(param)

  override val runExecutions: NonEmptyList[BaseRunExecution[_, _, _]] = children.flatMap(_.runExecutions)
}

sealed trait BaseRunExecution[AllArgs, RunArgs, Sender] extends Executions {
  def plugin: ScalaPlugin
  def permissions: Option[String]
  def help: CommandSender => Option[Text]
  def description: CommandSender => Option[Text]
  def userValidator: UserValidator[Sender]
  def allArgs: Parameter[AllArgs]
  def allArgsToRunArgs: AllArgs => RunArgs
  def runAsync: ExecutionContext => (Sender, RunArgs) => FutureOrNow[Either[String, Unit]]

  override def handleCommandAsync(
      source: CommandSender,
      cmdInput: List[RawCmdArg]
  )(using ec: ExecutionContext): FutureOrNow[ValidatedNel[(Int, CommandFailure, Usage), Unit]] = {
    // Nested so we can keep track of different errors,
    // and so we can do the first part of the command execution on the main thread
    val cmdExecutionRun: EitherT[FutureOrNow, (Int, CommandFailure), () => FutureOrNow[Either[String, Unit]]] = for
      _ <- EitherT.cond[FutureOrNow](
        permissions.forall(source.hasPermission),
        (),
        Int.MinValue -> CommandError("Not enough permissions to use this command")
      )
      parsedArgs <- EitherT.right[(Int, CommandFailure)](allArgs.parse(source).value.run(cmdInput))

      allArgs <- parsedArgs match {
        case (remaining, Left(e)) => EitherT.leftT[FutureOrNow, AllArgs]((cmdInput.length - remaining.length, e))
        case (remaining @ _ :: _, Right(res)) =>
          if remaining.length == 1 && remaining.head.content.trim.isEmpty
          then EitherT.rightT[FutureOrNow, (Int, CommandFailure)](res)
          else EitherT.leftT[FutureOrNow, AllArgs]((cmdInput.length - remaining.length, UnconsumedInput(remaining)))
        case (Nil, Right(result)) => EitherT.rightT[FutureOrNow, (Int, CommandFailure)](result)
      }

      user <- EitherT.fromEither(userValidator.validate(source).leftMap((Int.MaxValue, _)))
    yield () => runAsync(ec)(user, allArgsToRunArgs(allArgs))

    val fixedExecution: EitherT[FutureOrNow, (Int, CommandFailure), Either[String, Unit]] =
      EitherT(
        (cmdExecutionRun.value.value match {
          case Left(fut) =>
            FutureOrNow.fromFuture(fut.map(_.map(run => run()))(plugin.serverThreadExecutionContext))
          case Right(eval) => FutureOrNow.fromEval(eval.map(_.map(run => run())))
        }).flatMap {
          case Left(e)  => FutureOrNow.now(Left(e))
          case Right(v) => v.map(Right(_))
        }.memoize
      )

    fixedExecution.value.foreach {
      case Right(Left(e)) => source.sendMessage(t"$Red$e")
      case _              =>
    }

    fixedExecution.void.leftMap((parsed, error) => (parsed, error, allArgs.usage(source))).toValidatedNel
  }

  override def tabCompleteAsync(source: CommandSender, args: List[RawCmdArg])(
      using ExecutionContext
  ): FutureOrNow[List[String]] =
    if permissions.forall(source.hasPermission) then
      allArgs.suggestions(source).run(args).map { case (remaining, suggestions) =>
        if remaining.nonEmpty then Nil else suggestions
      }
    else FutureOrNow.now(Nil)

  override def flatten: Executions = this

  override def children: NonEmptyList[Executions] = NonEmptyList.one(this)

  override val runExecutions: NonEmptyList[BaseRunExecution[_, _, _]] = NonEmptyList.one(this)
}

case class RunExecution[AllArgs, RunArgs, Sender](
    plugin: ScalaPlugin,
    permissions: Option[String],
    help: CommandSender => Option[Text],
    description: CommandSender => Option[Text],
    userValidator: UserValidator[Sender],
    allArgs: Parameter[AllArgs],
    allArgsToRunArgs: AllArgs => RunArgs,
    run: (Sender, RunArgs) => Either[String, Unit]
) extends BaseRunExecution[AllArgs, RunArgs, Sender]:
  override def runAsync: ExecutionContext => (Sender, RunArgs) => FutureOrNow[Either[String, Unit]] = _ =>
    (s, r) => FutureOrNow.now(run(s, r))

  override def addArgFirst[Arg](parameter: Parameter[Arg]): Executions =
    new RunExecution[(Arg, AllArgs), RunArgs, Sender](
      plugin,
      permissions,
      help,
      description,
      userValidator,
      parameter ~ allArgs,
      t => allArgsToRunArgs(t._2),
      run
    )

case class RunAsyncExecution[AllArgs, RunArgs, Sender](
    plugin: ScalaPlugin,
    permissions: Option[String],
    help: CommandSender => Option[Text],
    description: CommandSender => Option[Text],
    userValidator: UserValidator[Sender],
    allArgs: Parameter[AllArgs],
    allArgsToRunArgs: AllArgs => RunArgs,
    runAsync: ExecutionContext => (Sender, RunArgs) => FutureOrNow[Either[String, Unit]]
) extends BaseRunExecution[AllArgs, RunArgs, Sender]:

  override def addArgFirst[Arg](parameter: Parameter[Arg]): Executions =
    new RunAsyncExecution[(Arg, AllArgs), RunArgs, Sender](
      plugin,
      permissions,
      help,
      description,
      userValidator,
      parameter ~ allArgs,
      t => allArgsToRunArgs(t._2),
      runAsync
    )
