package org.combinators.ep.generator.paradigm.control

import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{Command, Understands}
import org.combinators.ep.generator.paradigm.{AnyParadigm, DeclareVariable, IfThenElse}

case class PatternMatch[MethodBodyContext, Name, Expression](
  onValue: Expression,
  options: Map[(Name, Seq[Name]), Seq[Expression] => Generator[MethodBodyContext, Expression]]
) extends Command {
  type Result = Expression
}

case class Lambda[Name, Type, Context, Expression](variable: Name, tpe: Type, body: Expression => Generator[Context, Expression]) extends Command {
  type Result = Expression
}

trait Functional[Context] {
  val base: AnyParadigm

  import base.syntax._

  trait FunctionalCapabilities {
    implicit val canDeclareVar: Understands[Context, DeclareVariable[Name, Type, Generator[Context, Expression], Expression]]
    def declareVariable(name: Name, tpe: Type, expression: Generator[Context, Expression]): Generator[Context, Expression] =
      AnyParadigm.capabilitiy(DeclareVariable[Name, Type, Generator[Context, Expression], Expression](name, tpe, expression))

    implicit val canIfThenElse: Understands[Context, IfThenElse[Expression, Generator[Context, Expression], Generator[Context, Expression], Expression]]
    def ifThenElse(
        cond: Expression,
        ifBlock: Generator[Context, Expression],
        elseIfs: Seq[(Expression, Generator[Context, Expression])],
        elseBlock: Generator[Context, Expression]): Generator[Context, Expression] =
      AnyParadigm.capabilitiy(
        IfThenElse[Expression, Generator[Context, Expression], Generator[Context, Expression], Expression](
          cond,
          ifBlock,
          elseIfs,
          elseBlock
        ))

    implicit val canPatternMatch: Understands[Context, PatternMatch[Context, Name, Expression]]
    def patternMatch(
        onValue: Expression,
        options: Map[(Name, Seq[Name]), Seq[Expression] => Generator[Context, Expression]]
      ): Generator[Context, Expression] =
      AnyParadigm.capabilitiy(PatternMatch(onValue, options))

    implicit val canLambda: Understands[Context, Lambda[Name, Type, Context, Expression]]
    def lambda(variable: Name, tpe: Type, body: Expression => Generator[Context, Expression]): Generator[Context, Expression] =
      AnyParadigm.capabilitiy(Lambda(variable, tpe, body))
  }
  val functionalCapabilities: FunctionalCapabilities
}

object Functional {
  type WithBase[Ctxt, B <: AnyParadigm] = Functional[Ctxt] { val base: B }
}
