package org.combinators.ep.generator

import cats.data.State
import cats._
import cats.free.Free
import cats.free.Free.liftF

trait Command {
  type Result
}

object Command {
  type WithResult[R] = Command { type Result = R }
  type PerformCommand[C, X] = Perform[C, Command.WithResult[X]]
  type Generator[C, A] = Free[PerformCommand[C, *], A]

  case class Perform[Context, +Cmd <: Command](cmd: Cmd)(implicit val interpreter: Understands[Context, Cmd])

  implicit def perform[R, Cmd <: Command.WithResult[R], C](cmd: Cmd)(implicit interp: Understands[C, Cmd]): Generator[C, R] =
    liftF[PerformCommand[C, *], R](Perform[C, Cmd](cmd))

  def runGenerator[Context, Result](gen: Generator[Context, Result], inContext: Context): (Context, Result) = {
    val compiler: PerformCommand[Context, *] ~> State[Context, *] = new (PerformCommand[Context, *] ~> State[Context, *]) {
      def apply[A](fa: PerformCommand[Context, A]): State[Context, A] =
        State[Context, A](ctxt => fa.interpreter.perform(ctxt, fa.cmd))
    }
    gen.foldMap(compiler).run(inContext).value
  }

  implicit def monadInstance[C]: Monad[Generator[C, *]] =
    cats.free.Free.catsFreeMonadForFree[Command.PerformCommand[C, *]]
}