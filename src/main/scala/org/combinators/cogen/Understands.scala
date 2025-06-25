package org.combinators.cogen

/*DI:LI:AI*/

@scala.annotation.implicitNotFound(msg = "Context ${Context} does not understand ${Cmd}")
trait Understands[Context, Cmd <: Command] {
  /** Returns the updated context and the result of the command. */
  def perform(context: Context, command: Cmd): (Context, command.Result)
}

object Understands {
  def noop[Ctxt, Cmd <: Command.WithResult[Unit]]: Understands[Ctxt, Cmd] = new Understands[Ctxt, Cmd] {
    def perform(context: Ctxt, command: Cmd): (Ctxt, Unit) = (context, ())
  }
}
