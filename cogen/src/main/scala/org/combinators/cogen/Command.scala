package org.combinators.cogen

import cats._
import cats.data.State
import cats.free.Free
import cats.free.Free.liftF

/**
 * Every Command represents something delivered to a code generator for processing. Each Command has a result, which
 * can depend upon the kind of command. The result type is customized as needed. Every Command comes with a Context that
 * provides the means to interpret that Command.
 *
 * The interpretation of a Command can change based upon the context in which the command was created. The actual contexts
 * themselves depend on the code generation
 */
trait Command {
  type Result

  def interpret[Context, Self >: this.type <: Command.WithResult[Result]](implicit interp: Understands[Context, Self]): Command.Generator[Context, Result] = {
    val self: Self = this

    liftF[Command.Performable[Context, _], Result](Command.Performable[Context, Result, Self](self, interp))
  }
}

object Command {
  type WithResult[R] = Command { type Result = R }
  type Generator[C, A] = Free[Performable[C, _], A]

  sealed trait Performable[Context, R] {
    type Cmd <: Command.WithResult[R]
    val cmd: Cmd
    val interpreter: Understands[Context, Cmd]
  }
  object Performable {
    type Aux[Context, R, C <: Command.WithResult[R]] = Performable[Context, R] { type Cmd = C }
    def apply[Context, R, C <: Command.WithResult[R]](cmd: C, interpreter:  Understands[Context, C]): Aux[Context, R, C] = {
      case class P(cmd: C, interpreter: Understands[Context, C]) extends Performable[Context, R] { type Cmd = C }
      P(cmd, interpreter)
    }
  }

  def runGenerator[Context, Result](gen: Generator[Context, Result], inContext: Context): (Context, Result) = {
    val compiler: Performable[Context, _] ~> State[Context, _] = new (Performable[Context, _] ~> State[Context, _]) {
      def apply[A](fa: Performable[Context, A]): State[Context, A] =
        State[Context, A](ctxt => fa.interpreter.perform(ctxt, fa.cmd))
    }
    gen.foldMap(compiler).run(inContext).value
  }

  implicit def monadInstance[C]: Monad[Generator[C, _]] =
    cats.free.Free.catsFreeMonadForFree[Performable[C, _]]

  def lift[Context, T](value: T): Command.Generator[Context, T] = monadInstance.pure(value)
  def skip[Context]: Generator[Context, Unit] = lift(())
}