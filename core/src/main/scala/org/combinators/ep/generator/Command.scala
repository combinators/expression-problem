package org.combinators.ep.generator

import cats.data.State
import cats._
import cats.free.Free
import cats.free.Free.liftF

trait Command {
  type Result
  def interpret[Context, Self >: this.type <: Command.WithResult[Result]](implicit interp: Understands[Context, Self]): Command.Generator[Context, Result] = {
    val self: Self = this
    liftF[Command.Performable[Context, *], Result](new Command.Performable[Context, Result] {
      type Cmd = Self
      val cmd = self
      val interpreter = interp
    })
  }
}

object Command {
  type WithResult[R] = Command { type Result = R }
  type Generator[C, A] = Free[Performable[C, *], A]

  sealed trait Performable[Context, R] {
    type Cmd <: Command.WithResult[R]
    val cmd: Cmd
    val interpreter: Understands[Context, Cmd]
  }

  def runGenerator[Context, Result](gen: Generator[Context, Result], inContext: Context): (Context, Result) = {
    val compiler: Performable[Context, *] ~> State[Context, *] = new (Performable[Context, *] ~> State[Context, *]) {
      def apply[A](fa: Performable[Context, A]): State[Context, A] =
        State[Context, A](ctxt => fa.interpreter.perform(ctxt, fa.cmd))
    }
    gen.foldMap(compiler).run(inContext).value
  }

  implicit def monadInstance[C]: Monad[Generator[C, *]] =
    cats.free.Free.catsFreeMonadForFree[Performable[C, *]]
}