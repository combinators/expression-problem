package org.combinators.ep.generator

@scala.annotation.implicitNotFound(msg = "Context ${Context} does not understand ${Cmd}")
trait Understands[Context, Cmd <: Command] {
  def perform(context: Context, command: Cmd): (Context, command.Result)
}

object Understands {
  def noop[Ctxt, Cmd <: Command.WithResult[Unit]]: Understands[Ctxt, Cmd] = new Understands[Ctxt, Cmd] {
    def perform(context: Ctxt, command: Cmd): (Ctxt, Unit) = (context, ())
  }
}