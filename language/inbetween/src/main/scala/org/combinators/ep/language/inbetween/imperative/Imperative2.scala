package org.combinators.ep.language.inbetween.imperative   /*DI:LI:AI*/

import org.combinators.cogen.paradigm.{AnyParadigm, IfThenElse, control}
import org.combinators.cogen.paradigm.control.{AssignVariable, DeclareVariable, LiftExpression, Return, While}
import org.combinators.cogen.Command.Generator
import org.combinators.cogen.{Command, FileWithPath, Understands, paradigm}
import org.combinators.cogen.paradigm.control.{Return, DeclareVariable as DV, Imperative as Imp}
import org.combinators.ep.language.inbetween.any.AnyParadigm2


// Requires "recursive solution" to the EP, where Ctxt has a producer method and so this needs an EP solution, while
// talking about something which doesn't need to have one..
trait Imperative2(val _base: AnyParadigm2.WithAST[ImperativeAST]) {
  trait ImperativeInMethods extends control.Imperative[_base.ast.any.Method] {
    override val base: _base.type = _base
    import base.ast.any
    import base.ast.factory
    import base.ast.imperativeFactory

    type Ctxt = any.Method

    def pushToContext(context: Ctxt, stmt: any.Statement): Ctxt = {
      context.copy(statements = context.statements :+ stmt)
    }

    val imperativeCapabilities: ImperativeCapabilities = new ImperativeCapabilities {
      implicit val canDeclareVariable: Understands[Ctxt, DeclareVariable[any.Name, any.Type, Option[any.Expression], any.Expression]] = new Understands[Ctxt, DeclareVariable[any.Name, any.Type, Option[any.Expression], any.Expression]] {
        def perform(context: Ctxt, command: DeclareVariable[any.Name, any.Type, Option[any.Expression], any.Expression]): (Ctxt, any.Expression) = {
          (pushToContext(context, imperativeFactory.declareVariable(command.name, command.tpe, command.initialization)), imperativeFactory.variableReferenceExpression(command.name))
        }
      }

      implicit val canAssignVariable: Understands[Ctxt, AssignVariable[any.Expression, any.Statement]] =
        new Understands[Ctxt, AssignVariable[any.Expression, any.Statement]] {
          override def perform(context: Ctxt, command: AssignVariable[any.Expression, any.Statement]): (Ctxt, any.Statement) = {
            val assignStmt = imperativeFactory.assignVariable(command.variable, command.value)
            (context, assignStmt)
          }
        }
      implicit val canLiftExpression: Understands[Ctxt, LiftExpression[any.Expression, any.Statement]] =
        new Understands[Ctxt, LiftExpression[any.Expression, any.Statement]] {
          override def perform(context: Ctxt, command: LiftExpression[any.Expression, any.Statement]): (Ctxt, any.Statement) = {
            val liftStmt = imperativeFactory.liftExpression(command.expr)
            (context, liftStmt)
          }
        }
      implicit val canIfThenElse: Understands[Ctxt, IfThenElse[any.Expression, Generator[Ctxt, Unit], Option[Generator[Ctxt, Unit]], any.Statement]] =
        new Understands[Ctxt, IfThenElse[any.Expression, Generator[Ctxt, Unit], Option[Generator[Ctxt, Unit]], any.Statement]] {
          /** Returns the updated context and the result of the command. */
          override def perform(context: Ctxt, command: IfThenElse[any.Expression, Generator[Ctxt, Unit], Option[Generator[Ctxt, Unit]], any.Statement]): (Ctxt, any.Statement) = {
            def contextWithoutMethodBody(context: Ctxt) =
              context.copy(statements = Seq.empty)

            val (ifCtxt, _) = Command.runGenerator(command.ifBranch, contextWithoutMethodBody(context))
            val (elseIfCtxt, elseIfBranches) = command.elseIfBranches.foldLeft((ifCtxt, Seq.empty[(any.Expression, Seq[any.Statement])])) { case ((lastCtxt, elseIfBranches), (condition, elseIfBranch)) =>
              val (elseIfCtxt, _) = Command.runGenerator(elseIfBranch, contextWithoutMethodBody(lastCtxt))
              (elseIfCtxt, elseIfBranches :+ (condition, elseIfCtxt.statements))
            }
            val elseCtxt =
              command.elseBranch.map(elseBranch =>
                Command.runGenerator(elseBranch, contextWithoutMethodBody(elseIfCtxt))._1
              )
            val elseBranch = elseCtxt.map(_.statements).getOrElse(Seq.empty)
            val lastCtxt = elseCtxt.getOrElse(elseIfCtxt)

            val ifThenElseStmt = imperativeFactory.ifThenElse(command.condition, ifCtxt.statements, elseIfBranches, elseBranch)
            (lastCtxt.copy(statements = context.statements), ifThenElseStmt)
          }
        }
      implicit val canWhile: Understands[Ctxt, While[Ctxt, any.Expression, any.Statement]] = new Understands[Ctxt, While[Ctxt, any.Expression, any.Statement]] {
        def perform(context: Ctxt, command: While[Ctxt, any.Expression, any.Statement]): (Ctxt, any.Statement) = {
          def contextWithoutMethodBody(context: Ctxt) =
            context.copy(statements = Seq.empty)

          val (blockCtxt, _) = Command.runGenerator(command.block, contextWithoutMethodBody(context))
          val whileStmt = imperativeFactory.whileLoop(command.condition, blockCtxt.statements)
          (blockCtxt.copy(statements = context.statements), whileStmt)
        }
      }
      implicit val canReturn: Understands[Ctxt, Return[any.Expression, any.Statement]] = new Understands[Ctxt, Return[any.Expression, any.Statement]] {
        def perform(context: Ctxt, command: Return[any.Expression, any.Statement]): (Ctxt, any.Statement) = {
          val returnStmt = factory.returnExpression(command.exp)
          (context, returnStmt)
        }
      }
    }
  }
  val imperativeInMethods: ImperativeInMethods = new ImperativeInMethods {}
}

object Imperative2 {
  type WithBase[AST <: ImperativeAST, B <: AnyParadigm2.WithAST[AST]] = Imperative2 { val _base: B }
  trait WB[AST <: ImperativeAST, B <: AnyParadigm2.WithAST[AST]](override val _base: B) extends Imperative2 {}

  def apply[AST <: ImperativeAST, B <: AnyParadigm2.WithAST[AST]](_base: B): WithBase[AST, _base.type] = new WB[AST, _base.type](_base) with Imperative2(_base) {}
}