package org.combinators.ep.language.inbetween.imperative   /*DI:LI:AI*/

import org.combinators.ep.generator
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.control
import org.combinators.ep.generator.{Command, FileWithPath, Understands, paradigm}
import org.combinators.ep.generator.paradigm.control.{Return, Imperative => Imp, DeclareVariable => DV}
import org.combinators.ep.language.inbetween.any
import org.combinators.ep.language.inbetween.any.AnyParadigm


// Requires "recursive solution" to the EP, where Ctxt has a producer method and so this needs an EP solution, while
// talking about something which doesn't need to have one..
trait Imperative[FT <: FinalTypes, FactoryType <: Factory[FT]] extends Imp[any.Method[FT]] {
  val base: AnyParadigm.WithFT[FT, FactoryType]
  import base.factory

  type Ctxt = any.Method[FT]

  def pushToContext(context: Ctxt, stmt: any.Statement[FT]): Ctxt = {
    context.copy(statements = context.statements :+ stmt)
  }

  val imperativeCapabilities: ImperativeCapabilities = new ImperativeCapabilities {
    implicit val canDeclareVariable: Understands[Ctxt, DV[any.Name[FT], any.Type[FT], Option[any.Expression[FT]], any.Expression[FT]]] = new Understands[Ctxt, DV[any.Name[FT], any.Type[FT], Option[any.Expression[FT]], any.Expression[FT]]] {
      def perform(context: Ctxt, command: DV[any.Name[FT], any.Type[FT], Option[any.Expression[FT]], any.Expression[FT]]): (Ctxt, any.Expression[FT]) = {
        (pushToContext(context, factory.declareVariable(command.name, command.tpe, command.initialization)), factory.variableReferenceExpression(command.name))
      }
    }

    implicit val canAssignVariable: Understands[Ctxt, control.AssignVariable[any.Expression[FT], any.Statement[FT]]] =
      new Understands[Ctxt, control.AssignVariable[any.Expression[FT], any.Statement[FT]]] {
        override def perform(context: Ctxt, command: control.AssignVariable[any.Expression[FT], any.Statement[FT]]): (Ctxt, any.Statement[FT]) = {
          val assignStmt = factory.assignVariable(command.variable, command.value)
          (context, assignStmt)
        }
      }
    implicit val canLiftExpression: Understands[Ctxt, control.LiftExpression[any.Expression[FT], any.Statement[FT]]] =
      new Understands[Ctxt, control.LiftExpression[any.Expression[FT], any.Statement[FT]]] {
        override def perform(context: Ctxt, command: control.LiftExpression[any.Expression[FT], any.Statement[FT]]): (Ctxt, any.Statement[FT]) = {
          val liftStmt = factory.liftExpression(command.expr)
          (context, liftStmt)
        }
      }
    implicit val canIfThenElse: Understands[Ctxt, paradigm.IfThenElse[any.Expression[FT], Generator[Ctxt, Unit], Option[Generator[Ctxt, Unit]], any.Statement[FT]]] =
      new Understands[Ctxt, paradigm.IfThenElse[any.Expression[FT], Generator[Ctxt, Unit], Option[Generator[Ctxt, Unit]], any.Statement[FT]]] {
        /** Returns the updated context and the result of the command. */
        override def perform(context: Ctxt, command: paradigm.IfThenElse[any.Expression[FT], Generator[Ctxt, Unit], Option[Generator[Ctxt, Unit]], any.Statement[FT]]): (Ctxt, any.Statement[FT]) = {
          def contextWithoutMethodBody(context: Ctxt) =
            context.copy(statements = Seq.empty)

          val (ifCtxt, _) = Command.runGenerator(command.ifBranch, contextWithoutMethodBody(context))
          val (elseIfCtxt, elseIfBranches) = command.elseIfBranches.foldLeft((ifCtxt, Seq.empty[(any.Expression[FT], Seq[any.Statement[FT]])])) { case ((lastCtxt, elseIfBranches), (condition, elseIfBranch)) =>
            val (elseIfCtxt, _) = Command.runGenerator(elseIfBranch, contextWithoutMethodBody(lastCtxt))
            (elseIfCtxt, elseIfBranches :+ (condition, elseIfCtxt.statements))
          }
          val elseCtxt =
            command.elseBranch.map(elseBranch =>
              Command.runGenerator(elseBranch, contextWithoutMethodBody(elseIfCtxt))._1
            )
          val elseBranch = elseCtxt.map(_.statements).getOrElse(Seq.empty)
          val lastCtxt = elseCtxt.getOrElse(elseIfCtxt)

          val ifThenElseStmt = factory.ifThenElse(command.condition, ifCtxt.statements, elseIfBranches, elseBranch)
          (lastCtxt.copy(statements = context.statements), ifThenElseStmt)
        }
      }
    implicit val canWhile: Understands[Ctxt, control.While[Ctxt, any.Expression[FT], any.Statement[FT]]] = new Understands[Ctxt, control.While[Ctxt, any.Expression[FT], any.Statement[FT]]] {
      def perform(context: Ctxt, command: control.While[Ctxt, any.Expression[FT], any.Statement[FT]]): (Ctxt, any.Statement[FT]) = {
        def contextWithoutMethodBody(context: Ctxt) =
          context.copy(statements = Seq.empty)

        val (blockCtxt, _) = Command.runGenerator(command.block, contextWithoutMethodBody(context))
        val whileStmt = factory.whileLoop(command.condition, blockCtxt.statements)
        (blockCtxt.copy(statements = context.statements), whileStmt)
      }
    }
    implicit val canReturn: Understands[Ctxt, Return[any.Expression[FT], any.Statement[FT]]] = new Understands[Ctxt, Return[any.Expression[FT], any.Statement[FT]]] {
      def perform(context: Ctxt, command: Return[any.Expression[FT], any.Statement[FT]]): (Ctxt, any.Statement[FT]) = {
        val returnStmt = factory.returnExpression(command.exp)
        (context, returnStmt)
      }
    }
  }

}

object Imperative {
  type WithBase[FT <: FinalTypes, FactoryType <: Factory[FT], B <: AnyParadigm.WithFT[FT, FactoryType]] = Imperative[FT, FactoryType] { val base: B }
  def apply[FT <: FinalTypes, FactoryType <: Factory[FT], B <: AnyParadigm.WithFT[FT, FactoryType]](_base: B): WithBase[FT, FactoryType, _base.type] = new Imperative[FT, FactoryType] {
    val base: _base.type = _base
  }
}