package net.katsstuff.bukkit.katlib.command

import scala.collection.mutable

import cats.data.NonEmptyList
import net.katsstuff.bukkit.katlib.ScalaPlugin
import net.katsstuff.bukkit.katlib.text.*
import org.bukkit.command.CommandSender

class ExecutionBuilder(using val plugin: ScalaPlugin):

  private val currentExecutions = mutable.Buffer[Executions]()

  def executions: AggExecutions = AggExecutions(NonEmptyList.fromListUnsafe(currentExecutions.toList))

  def addExecutionToBuilder(executions: Executions): Unit =
    currentExecutions += executions

end ExecutionBuilder

inline def addExecution[Args, Sender](
    args: Parameter[Args] = Parameters.unit,
    sender: UserValidator[Sender] = Senders.commandSender,
    permissions: String = "",
    help: CommandSender => Option[Text] = _ => None,
    description: CommandSender => Option[Text] = _ => None
)(
    f: (Sender, Args) => Either[String, Unit]
)(using builder: ExecutionBuilder): Unit =
  import builder.plugin
  builder.addExecutionToBuilder(execution(args, sender, permissions, help, description)(f))

inline def addWithArg[Args](arg: Parameter[Args])(f: Parameter[Args] => Unit)(using ExecutionBuilder): Unit =
  f(arg)

def addSubCommand(name: String, names: String*)(children: ExecutionBuilder ?=> Unit)(
    using outerBuilder: ExecutionBuilder
): Unit =
  import outerBuilder.plugin
  val innerBuilder = new ExecutionBuilder
  children(using innerBuilder)
  outerBuilder.addExecutionToBuilder(
    Command.subCommand(name, names*)(
      innerBuilder.executions.executions.head,
      innerBuilder.executions.executions.tail*
    )
  )
