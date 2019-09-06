package org.combinators.ep.generator

@scala.annotation.implicitNotFound(msg = "Context ${Context} does not understand ${Cmd}")
trait Understands[Context, Cmd <: Command] {
  def perform(context: Context, command: Cmd): (Context, command.Result)
}