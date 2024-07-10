package org.combinators.ep.language.inbetween.functional.control

import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{Command, Understands, paradigm}
import org.combinators.ep.generator.paradigm.{IfThenElse, control}
import org.combinators.ep.generator.paradigm.control.{PatternMatch, Functional => Fun}
import org.combinators.ep.language.inbetween.any
import org.combinators.ep.language.inbetween.any.AnyParadigm

trait Functional[FT <: FinalTypes, FactoryType <: Factory[FT]] extends Fun[any.Method[FT]] {
  val base: AnyParadigm.WithFT[FT, FactoryType]

  import base.factory

  type Ctxt = any.Method[FT]

  val lambdaCapabilities: LambdaCapabilities = new LambdaCapabilities {
    implicit val canLambda: Understands[any.Method[FT], control.Lambda[any.Name[FT], any.Type[FT], any.Method[FT], any.Expression[FT]]] = new Understands[any.Method[FT], control.Lambda[any.Name[FT], any.Type[FT], any.Method[FT], any.Expression[FT]]] {
      override def perform(context: any.Method[FT], command: control.Lambda[any.Name[FT], any.Type[FT], any.Method[FT], any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
        val args = command.variables.map { case (pname, _tpe) => (pname, factory.argumentExpression(pname)) }.toMap
        val (updatedCtxt, body) = Command.runGenerator(command.body(args), context)
        (updatedCtxt, factory.lambda(command.variables, body))
      }
    }
  }

  val functionalCapabilities: FunctionalCapabilities = new FunctionalCapabilities {
    implicit val canDeclareVar: Understands[any.Method[FT], paradigm.DeclareVariable[any.Name[FT], any.Type[FT], any.Expression[FT], (any.Expression[FT] => any.Expression[FT]) => any.Expression[FT]]] =      
      new Understands[any.Method[FT], paradigm.DeclareVariable[any.Name[FT], any.Type[FT], any.Expression[FT], (any.Expression[FT] => any.Expression[FT]) => any.Expression[FT]]] {
        override def perform(context: any.Method[FT], command: paradigm.DeclareVariable[any.Name[FT], any.Type[FT], any.Expression[FT], (any.Expression[FT] => any.Expression[FT]) => any.Expression[FT]]): (any.Method[FT], (any.Expression[FT] => any.Expression[FT]) => any.Expression[FT]) = {
          val res: (any.Expression[FT] => any.Expression[FT]) => any.Expression[FT] = inExpressionFn => {
            inExpressionFn(factory.argumentExpression(command.name))
          }
          (context, res)
        }
    }
    implicit val canIfThenElse: Understands[any.Method[FT], paradigm.IfThenElse[any.Expression[FT], Generator[any.Method[FT], any.Expression[FT]], Generator[any.Method[FT], any.Expression[FT]], any.Expression[FT]]] =
      new Understands[any.Method[FT], paradigm.IfThenElse[any.Expression[FT], Generator[any.Method[FT], any.Expression[FT]], Generator[any.Method[FT], any.Expression[FT]], any.Expression[FT]]] {
        override def perform(context: any.Method[FT], command: paradigm.IfThenElse[any.Expression[FT], Generator[any.Method[FT], any.Expression[FT]], Generator[any.Method[FT], any.Expression[FT]], any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
          val (startingCtxt, ifBranchExp) = Command.runGenerator[any.Method[FT], any.Expression[FT]](command.ifBranch, context)
          val (elseIfCtxt, elseIfExprs) = command.elseIfBranches.foldLeft[(any.Method[FT], Seq[(any.Expression[FT], any.Expression[FT])])]((startingCtxt, Seq.empty)){ case ((ctxt, exprs), elseIfBranch) =>
            val (nextCtxt, elseIfExpr) = Command.runGenerator[any.Method[FT], any.Expression[FT]](elseIfBranch._2, ctxt)
            (nextCtxt, exprs :+ (elseIfBranch._1, elseIfExpr))
          }
          val (elseCtxt, elseExpr) = Command.runGenerator[any.Method[FT], any.Expression[FT]](command.elseBranch, elseIfCtxt)
          (elseCtxt, factory.ifThenElse(command.condition, ifBranchExp, elseIfExprs, elseExpr))
        }
      }
    implicit val canPatternMatch: Understands[any.Method[FT], control.PatternMatch[any.Method[FT], any.Name[FT], any.Expression[FT]]] = ???
  }
   
}

object Functional {
  type WithBase[FT <: FinalTypes, FactoryType <: Factory[FT], B <: AnyParadigm.WithFT[FT, FactoryType]] = Functional[FT, FactoryType] { val base: B }
  def apply[FT <: FinalTypes, FactoryType <: Factory[FT], B <: AnyParadigm.WithFT[FT, FactoryType]](_base: B): WithBase[FT, FactoryType, _base.type] = new Functional[FT, FactoryType] {
    val base: _base.type = _base
  }
}