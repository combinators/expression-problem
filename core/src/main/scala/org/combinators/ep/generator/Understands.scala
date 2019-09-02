package org.combinators.ep.generator

import org.combinators.ep.generator.Command.Perform

@scala.annotation.implicitNotFound(msg = "Context ${Context} does not understand ${Cmd}")
trait Understands[Context, Cmd <: Command] {
  def perform(context: Context, command: Cmd): (Context, command.Result)
}