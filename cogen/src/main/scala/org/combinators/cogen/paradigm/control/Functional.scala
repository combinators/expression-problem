package org.combinators.cogen.paradigm.control

/*DI:LI:AI*/

import org.combinators.cogen.paradigm.{AnyParadigm, Apply, FreshName, IfThenElse, Reify}
import org.combinators.cogen.{TypeRep, Command, Understands}
import Command.Generator

case class PatternMatch[MethodBodyContext, PatternContext, Expression](
  onValue: Expression,
  options: Seq[(Generator[PatternContext, Expression], Seq[Expression] => Generator[MethodBodyContext, Expression])]
) extends Command {
  type Result = Expression
}

case class PatternVariable[Name, Expression](
  name: Name
) extends Command {
  type Result = Expression
}

case class ConstructorPattern[Type, Name](
  tpe: Type,
  constructor: Name
)

case class DeclareFunVariable[Ctxt, Name, Type, Init, Res](name: Name, tpe: Type, initialization: Init, inBlk: Res => Generator[Ctxt, Res]) extends Command {
  type Result = Res
}

trait Functional[Context] extends Lambdas[Context] {
  val base: AnyParadigm

  import base.syntax._
  
  type PatternContext

  trait FunctionalCapabilities {
    implicit val canDeclareVar: Understands[Context, DeclareFunVariable[Context, Name, Type, Expression, Expression]]
    def declareVariable(name: Name, tpe: Type, init: Expression)(inBlock: Expression => Generator[Context, Expression]): Generator[Context, Expression] =
      AnyParadigm.capability(DeclareFunVariable(name, tpe, init, inBlock))

    // ml: let rec local_fun = x => local_fun (x - 1)
    // haskell: let local_fun = \ x -> local_fun (x - 1)
    // scala: def local_fun: Int => Int = (x : Int) => local_fun (x - 1)
    implicit val canDeclareRecVar: Understands[Context, DeclareFunVariable[Context, Name, Type, Expression => Generator[Context, Expression], Expression]]
    def declareRecursiveVariable(name: Name, tpe: Type, init: Expression => Generator[Context, Expression])(inBlock: Expression => Generator[Context, Expression]): Generator[Context, Expression] =
      AnyParadigm.capability(DeclareFunVariable(name, tpe, init, inBlock))


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

    implicit val canPatternMatch: Understands[Context, PatternMatch[Context, PatternContext, Expression]]
    def patternMatch(
        onValue: Expression,
        options: Seq[(Generator[PatternContext, Expression], Seq[Expression] => Generator[Context, Expression])]
      ): Generator[Context, Expression] =
      AnyParadigm.capability(PatternMatch(onValue, options))
  }
  val functionalCapabilities: FunctionalCapabilities

  trait PatternCapabilities {
    implicit val canPatternVariable: Understands[PatternContext, PatternVariable[Name, Expression]]
    def patternVariable(basedOn: Name): Generator[PatternContext, Expression] =
      AnyParadigm.capability(PatternVariable[Name, Expression](basedOn))

    implicit def canReifyInPattern[T]: Understands[PatternContext, Reify[T, Expression]]
    def reify[T](tpe: TypeRep.OfHostType[T], value: T): Generator[PatternContext, Expression] =
      AnyParadigm.capability(Reify[T, Expression](tpe, value))

    implicit val canApplyConstructorPattern: Understands[PatternContext, Apply[ConstructorPattern[Type, Name], Generator[PatternContext, Expression], Expression]]
    def applyConstructorPattern(
      constructor: ConstructorPattern[Type, Name],
      arguments: Seq[Generator[PatternContext, Expression]]): Generator[PatternContext, Expression] =
      AnyParadigm.capability(Apply[ConstructorPattern[Type, Name], Generator[PatternContext, Expression], Expression](constructor, arguments))
  }
  val patternCapabilities: PatternCapabilities
}

object Functional {
  type WithBase[Ctxt, B <: AnyParadigm] = Functional[Ctxt] { val base: B }
}

