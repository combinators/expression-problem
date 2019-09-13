package org.combinators.ep.generator.paradigm.control

import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{Command, Understands}
import org.combinators.ep.generator.paradigm.{AnyParadigm, DeclareVariable, IfThenElse}

import cats.implicits._
import cats.free.Free._

case class AssignVariable[Expression, Statement](name: String, value: Expression) extends Command {
  type Result = Statement
}

case class While[Ctxt, Expression, Statement](condition: Expression, block: Generator[Ctxt, Unit]) extends Command {
  type Result = Statement
}

case class Return[Expression, Statement](exp: Expression) extends Command {
  type Result = Statement
}

trait Imperative[Context] {
  val base: AnyParadigm

  import base.syntax._
  import AnyParadigm.syntax._

  trait ImperativeCapabilities {
    implicit val canDeclareVariable: Understands[Context, DeclareVariable[Type, Option[Expression], Statement]]
    def declareVar(name: String, tpe: Type, init: Option[Expression] = None): Generator[Context, Statement] =
      AnyParadigm.capabilitiy(DeclareVariable[Type, Option[Expression], Statement](name, tpe, init))

    implicit val canAssignVariable: Understands[Context, AssignVariable[Expression, Statement]]
    def assignVar(name: String, value: Expression): Generator[Context, Statement] =
      AnyParadigm.capabilitiy(AssignVariable[Expression, Statement](name, value))

    implicit val canIfThenElse: Understands[Context, IfThenElse[Expression, Generator[Context, Unit], Option[Generator[Context, Unit]], Statement]]
    def ifThenElse(
        condition: Expression,
        ifBranch: Generator[Context, Unit],
        elseIfs: Seq[(Expression, Generator[Context, Unit])],
        elseBranch: Option[Generator[Context, Unit]] = None
      ): Generator[Context, Statement] =
      AnyParadigm.capabilitiy(IfThenElse[Expression, Generator[Context, Unit], Option[Generator[Context, Unit]], Statement](
        condition, ifBranch, elseIfs, elseBranch
      ))

    implicit val canWhile: Understands[Context, While[Context, Expression, Statement]]
    def whileLoop(condition: Expression, block: Generator[Context, Unit]): Generator[Context, Statement] =
      AnyParadigm.capabilitiy(While[Context, Expression, Statement](condition, block))

    implicit def canReturn: Understands[Context, Return[Expression, Statement]]
    def returnStmt(exp: Expression): Generator[Context, Statement] =
      AnyParadigm.capabilitiy(Return[Expression, Statement](exp))

    def liftReturnStmt(expGen: Generator[Context, Expression]): Generator[Context, Statement] = {
      for {
        exp <- expGen
        res <- returnStmt(exp)
      } yield res
    }
  }
  val imperativeCapabilities: ImperativeCapabilities

}

object Imperative {
  type WithBase[Ctxt, B <: AnyParadigm] = Imperative[Ctxt] { val base: B }
}
