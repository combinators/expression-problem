package org.combinators.cogen.paradigm.control

/*DI:LI:AI*/

import org.combinators.cogen.paradigm.AnyParadigm
import org.combinators.cogen.{Command, Understands}
import Command.Generator

case class Lambda[Name, Type, Context, Expression](variables: Seq[(Name,Type)], body: Map[Name,Expression] => Generator[Context, Expression]) extends Command {
  type Result = Expression
}

trait Lambdas[Context] {
  val base: AnyParadigm
  import base.syntax._

  trait LambdaCapabilities {
    implicit val canLambda: Understands[Context, Lambda[Name, Type, Context, Expression]]
    def lambda(variables: Seq[(Name, Type)], body: Map[Name,Expression] => Generator[Context, Expression]): Generator[Context, Expression] =
      AnyParadigm.capability(Lambda(variables, body))
  }

  val lambdaCapabilities: LambdaCapabilities
}
