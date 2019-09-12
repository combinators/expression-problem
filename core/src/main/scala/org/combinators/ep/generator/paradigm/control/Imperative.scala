package org.combinators.ep.generator.paradigm.control

import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{Command, Understands}
import org.combinators.ep.generator.paradigm.{AnyParadigm, DeclareVariable, IfThenElse}

import cats.implicits._
import cats.free.Free._

case class AssignVariable[Expression, Statement](name: String, value: Expression) extends Command {
  type Result = Statement
}

case class While[Expression, Statement](condition: Expression, block: Seq[Statement]) extends Command {
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

    implicit val canIfThenElse: Understands[Context, IfThenElse[Expression, Seq[Statement], Option[Seq[Statement]], Statement]]
    def ifThenElse(
        condition: Expression,
        ifBranch: Seq[Statement],
        elseIfs: Seq[(Expression, Seq[Statement])],
        elseBranch: Option[Seq[Statement]] = None
      ): Generator[Context, Statement] =
      AnyParadigm.capabilitiy(IfThenElse[Expression, Seq[Statement], Option[Seq[Statement]], Statement](
        condition, ifBranch, elseIfs, elseBranch
      ))

    implicit val canWhile: Understands[Context, While[Expression, Statement]]
    def whileLoop(condition: Expression, block: Seq[Statement]): Generator[Context, Statement] =
      AnyParadigm.capabilitiy(While[Expression, Statement](condition, block))

    def liftIfThenElse(
        condition: Generator[Context, Expression],
        ifBranch: Generator[Context, Seq[Statement]],
        elseIfs: Seq[(Generator[Context,Expression], Generator[Context,Seq[Statement]])],
        elseBranch: Option[Generator[Context,Seq[Statement]]] = None
      ): Generator[Context, Statement] = {
      for {
        cond <- condition
        ifPart <- ifBranch
        elseIfPart <- forEach (elseIfs) { case (c, b) => for {eic <- c; eib <- b} yield (eic, eib) }
        elsePart <- elseBranch.sequence
        result <- ifThenElse(cond, ifPart, elseIfPart, elsePart)
      } yield result
    }

    def liftWhile(
        condition: Generator[Context, Expression], block: Generator[Context, Seq[Statement]]
      ): Generator[Context, Statement] = {
      for {
        cond <- condition
        blk <- block
        result <- whileLoop(cond, blk)
      } yield result
    }

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
