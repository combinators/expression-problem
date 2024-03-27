package org.combinators.ep.generator.paradigm.control   /*DI:LI:AI*/

import org.combinators.ep.generator.{Command, Understands}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.AnyParadigm

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
