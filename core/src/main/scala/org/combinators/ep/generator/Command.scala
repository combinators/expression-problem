package org.combinators.ep.generator   /*DI:LI:AI*/

import cats.data.State
import cats._
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

  def lift[Context, T](value: T): Command.Generator[Context, T] = monadInstance.pure(value)
  def skip[Context]: Generator[Context, Unit] = lift(())
}