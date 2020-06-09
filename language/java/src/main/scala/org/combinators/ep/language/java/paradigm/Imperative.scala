package org.combinators.ep.language.java.paradigm    /*DI:LD:AI*/

import com.github.javaparser.ast.expr.{AssignExpr, NameExpr, VariableDeclarationExpr}
import com.github.javaparser.ast.stmt.{BlockStmt, ExpressionStmt, IfStmt, ReturnStmt, WhileStmt}
import org.combinators.ep.generator.{Command, Understands}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.{DeclareVariable, IfThenElse}
import org.combinators.ep.generator.paradigm.control.{Imperative => Imp, _}
import org.combinators.ep.language.java.{CtorCtxt, MethodBodyCtxt}

trait Imperative[Ctxt, AP <: AnyParadigm] extends Imp[Ctxt] {

  val base: AP
  val manip: Imperative.BlockContextManipulator[Ctxt]

  import base.syntax._

  object imperativeCapabilities extends ImperativeCapabilities {
    implicit val canDeclareVariable: Understands[Ctxt, DeclareVariable[Name, Type, Option[Expression], Expression]] =
      new Understands[Ctxt, DeclareVariable[Name, Type, Option[Expression], Expression]] {
        def perform(context: Ctxt, command: DeclareVariable[Name, Type, Option[Expression], Expression]): (Ctxt, Expression) = {
          val decl = new VariableDeclarationExpr(command.tpe, command.name.toAST.toString)
          val withAssignment =
            if (command.initialization.isDefined) {
              new AssignExpr(decl, command.initialization.get.clone(), AssignExpr.Operator.ASSIGN)
            } else {
              decl
            }
          val nextBlock = manip.getBlock(context).clone()
          nextBlock.addStatement(withAssignment)
          (manip.copyWithBlock(context, nextBlock), new NameExpr(command.name.toAST))
        }
      }

    implicit val canAssignVariable: Understands[Ctxt, AssignVariable[Expression, Statement]] =
      new Understands[Ctxt, AssignVariable[Expression, Statement]] {
        def perform(context: Ctxt, command: AssignVariable[Expression, Statement]): (Ctxt, Statement) = {
          (context, new ExpressionStmt(new AssignExpr(command.variable.clone(), command.value.clone(), AssignExpr.Operator.ASSIGN)))
        }
      }

    // tacitly converts an expression into a stand-alone statement.
    implicit val canLiftExpression: Understands[Ctxt, LiftExpression[Expression, Statement]] =
      new Understands[Ctxt, LiftExpression[Expression, Statement]] {
        def perform(context: Ctxt, command: LiftExpression[Expression, Statement]): (Ctxt, Statement) = {
          (context, new ExpressionStmt(command.expr.clone()))
        }
      }

    implicit val canReturn: Understands[Ctxt, Return[Expression, Statement]] =
      new Understands[Ctxt, Return[Expression, Statement]] {
        def perform(context: Ctxt, command: Return[Expression, Statement]): (Ctxt, Statement) = {
          (context, new ReturnStmt(command.exp.clone()))
        }
      }

    implicit val canIfThenElse: Understands[Ctxt, IfThenElse[Expression, Generator[Ctxt, Unit], Option[Generator[Ctxt, Unit]], Statement]] =
      new Understands[Ctxt, IfThenElse[Expression, Generator[Ctxt, Unit], Option[Generator[Ctxt, Unit]], Statement]] {
        def perform(
          context: Ctxt,
          command: IfThenElse[Expression, Generator[Ctxt, Unit], Option[Generator[Ctxt, Unit]], Statement]
        ): (Ctxt, Statement) = {
          val (startCtxt, _) = Command.runGenerator(command.ifBranch, manip.nextBlockContext(context))
          val iteStmt: IfStmt = new IfStmt()
          iteStmt.setCondition(command.condition)
          iteStmt.setThenStmt(manip.getBlock(startCtxt))
          val (beforeElseCtxt, beforeElseIteStmt) =
            command.elseIfBranches.foldLeft((manip.nextBlockContext(startCtxt), iteStmt)) {
              case ((ctxt, iteStmt), (cond, gen)) =>
                val (nextCtxt, _) = Command.runGenerator(gen, manip.nextBlockContext(ctxt))
                val nextIte = new IfStmt()
                nextIte.setCondition(cond)
                nextIte.setThenStmt(manip.getBlock(nextCtxt))
                iteStmt.setElseStmt(nextIte)
                (nextCtxt, nextIte)
            }
          val afterElseCtxt =
            command.elseBranch
              .map { elseGen =>
                val (resCtxt, _) = Command.runGenerator(elseGen, manip.nextBlockContext(beforeElseCtxt))
                beforeElseIteStmt.setElseStmt(manip.getBlock(resCtxt))
                resCtxt
              }.getOrElse(beforeElseCtxt)
          (manip.copyWithBlock(afterElseCtxt, manip.getBlock(context)), iteStmt.clone())
        }
      }
    implicit val canWhile: Understands[Ctxt, While[Ctxt, Expression, Statement]] =
      new Understands[Ctxt, While[Ctxt, Expression, Statement]] {
        def perform(context: Ctxt, command: While[Ctxt, Expression, Statement]): (Ctxt, Statement) = {
          val (whileCtxt, _) = Command.runGenerator(command.block, manip.nextBlockContext(context))
          val whileStmt = new WhileStmt()
          whileStmt.setCondition(command.condition)
          whileStmt.setBody(manip.getBlock(whileCtxt))
          (manip.copyWithBlock(whileCtxt, manip.getBlock(context)), whileStmt)
        }
      }
  }
}

object Imperative {
  trait BlockContextManipulator[Ctxt] {
    def getBlock(ctxt: Ctxt): BlockStmt
    def copyWithBlock(ctxt: Ctxt, blockStmt: BlockStmt): Ctxt
    def nextBlockContext(ctxt: Ctxt): Ctxt = copyWithBlock(ctxt, new BlockStmt())
  }

  def inMethodContext[AP <: AnyParadigm](base: AP): Imperative[MethodBodyCtxt, base.type] = {
    val b: base.type = base
    new Imperative[MethodBodyCtxt, b.type] {
      val base: b.type = b
      val manip: BlockContextManipulator[MethodBodyCtxt] =
        new BlockContextManipulator[MethodBodyCtxt] {
          def getBlock(ctxt: MethodBodyCtxt): BlockStmt = ctxt.method.getBody.get()
          def copyWithBlock(ctxt: MethodBodyCtxt, blockStmt: BlockStmt): MethodBodyCtxt = {
            val newMethod = ctxt.method.clone()
            newMethod.setBody(blockStmt.clone())
            ctxt.copy(method = newMethod)
          }
        }
    }
  }

  def inConstructorContext[AP <: AnyParadigm](base: AP): Imperative[CtorCtxt, base.type] = {
    val b: base.type = base
    new Imperative[CtorCtxt, b.type] {
      val base: b.type = b
      val manip: BlockContextManipulator[CtorCtxt] =
        new BlockContextManipulator[CtorCtxt] {
          def getBlock(ctxt: CtorCtxt): BlockStmt = ctxt.ctor.getBody
          def copyWithBlock(ctxt: CtorCtxt, blockStmt: BlockStmt): CtorCtxt = {
            val newCtor = ctxt.ctor.clone()
            newCtor.setBody(blockStmt.clone())
            ctxt.copy(ctor = newCtor)
          }
        }
    }
  }
}