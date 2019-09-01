package org.combinators.ep.generator

import org.combinators.ep.generator.Command.Perform

trait Understands[Context, Cmd <: Command] {
  def perform(context: Context, command: Cmd): (Context, command.Result)
}

object Understands {
  def ![Context, Cmd <: Command](context: Context, instruction: Perform[Context, Cmd])
    (implicit interpreter: Understands[Context, Cmd]): (Context, instruction.cmd.Result) =
    interpreter.perform(context, instruction.cmd)
}
