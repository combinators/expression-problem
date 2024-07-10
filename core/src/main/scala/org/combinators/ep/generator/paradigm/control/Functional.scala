package org.combinators.ep.generator.paradigm.control   /*DI:LI:AI*/

import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{Command, Understands}
import org.combinators.ep.generator.paradigm.{AnyParadigm, DeclareVariable, IfThenElse}

case class PatternMatch[MethodBodyContext, PatterExpression, Expression](
  onValue: Expression,
  options: Seq[(PatterExpression, Seq[Expression] => Generator[MethodBodyContext, Expression])]
) extends Command {
  type Result = Expression
}

case class PatternVariable[Name, PatternExpression](name: Name) {
  type Result = PatternExpression
}

trait Functional[Context] extends Lambdas[Context] {
  val base: AnyParadigm

  import base.syntax._

  trait FunctionalCapabilities {
    implicit val canDeclareVar: Understands[Context, DeclareVariable[Name, Type, Expression, (Expression => Expression) => Expression]]
    def declareVariable(name: Name, tpe: Type, init: Expression): Generator[Context, (Expression => Expression) => Expression] =
      AnyParadigm.capability(DeclareVariable[Name, Type, Expression, (Expression => Expression) => Expression](name, tpe, init))
 
    implicit val canIfThenElse: Understands[Context, IfThenElse[Expression, Generator[Context, Expression], Generator[Context, Expression], Expression]]
    def ifThenElse(
        cond: Expression,
        ifBlock: Generator[Context, Expression],
        elseIfs: Seq[(Expression, Generator[Context, Expression])],
        elseBlock: Generator[Context, Expression]): Generator[Context, Expression] =
      AnyParadigm.capability(
        IfThenElse[Expression, Generator[Context, Expression], Generator[Context, Expression], Expression](
          cond,
          ifBlock,
          elseIfs,
          elseBlock
        ))
        
    // TODO: Figure out syntax category extension for PatternExpression
    implicit val canPatternVariable: Understands[Context, PatternVariable[Name, PatternExpression]]
    def patternVariable(name: Name): Generator[Context, PatternExpression] =
      AnyParadigm.capability(PatternVariable(name))

    implicit val canPatternMatch: Understands[Context, PatternMatch[Context, PatternExpression, Expression]]
    def patternMatch(
        onValue: Expression,
        options: Seq[(PatternExpression, Seq[Expression] => Generator[Context, Expression])]
      ): Generator[Context, Expression] =
      AnyParadigm.capability(PatternMatch(onValue, options))
  }
  val functionalCapabilities: FunctionalCapabilities
}

object Functional {
  type WithBase[Ctxt, B <: AnyParadigm] = Functional[Ctxt] { val base: B }
}
