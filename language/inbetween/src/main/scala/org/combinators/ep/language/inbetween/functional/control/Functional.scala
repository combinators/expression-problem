package org.combinators.ep.language.inbetween.functional.control

import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{Command, Understands, paradigm}
import org.combinators.ep.generator.paradigm.{Apply, IfThenElse, Reify, control}
import org.combinators.ep.generator.paradigm.control.{PatternMatch, Functional => Fun}
import org.combinators.ep.language.inbetween.any
import org.combinators.ep.language.inbetween.any.AnyParadigm

trait Functional[FT <: FinalTypes, FactoryType <: Factory[FT]] extends Fun[any.Method[FT], PatternContext[FT]] {
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
    implicit val canPatternMatch: Understands[any.Method[FT], control.PatternMatch[any.Method[FT], any.Name[FT], any.Expression[FT]]] = new Understands[any.Method[FT], control.PatternMatch[any.Method[FT], any.Name[FT], any.Expression[FT]]] {
      override def perform(context: any.Method[FT], command: control.PatternMatch[any.Method[FT], any.Name[FT], any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
        val (finalCtxt, cases) = command.options.foldLeft((context, Seq.empy[(any.Expression[FT], any.Expression[FT])])) {
          case ((ctxt, cases), option) =>
            val (patternCtxt, patternExp) = Command.runGenerator(option._1, factory.convert(ctxt).emptyPatternCtxt)
            val patternVariables = patternCtxt.variables.distinct.map(name => factory.argumentExpression(name))
            val branchGen = option._2(patternVariables)
            val (nextCtxt, branchExp) = Command.runGenerator(branchGen, ctxt)
            (nextCtxt, cases :+ (patternExp, branchExp))
          }
        (finalCtxt, factory.patternMatch(command.onValue, cases))
      }
    }
  }

  override val patternCapabilities: PatternCapabilities = new PatternCapabilities {
    implicit val canPatternVariable: Understands[PatternContext[FT], control.PatternVariable[any.Name[FT], any.Expression[FT]]] = new Understands[PatternContext[FT], control.PatternVariable[any.Name[FT], any.Expression[FT]]] {
      override def perform(context: PatternContext[FT], command: control.PatternVariable[any.Name[FT], any.Expression[FT]]): (PatternContext[FT], any.Expression[FT]) = {
        (context.copy(context.variables :+ command.name), factory.patternVariable(command.name))
      }
    }
    implicit def canReifyInPattern[T]: Understands[PatternContext[FT], Reify[T, any.Expression[FT]]] = {
      def perform(context: PatternContext[FT], command: Reify[T, Expression[FT]]): (PatternContext[FT], Expression[FT]) = {
        (context, context.reify(command.tpe, command.value))
      }
    }
    
    implicit val canApplyConstructorPattern: Understands[PatternContext[FT], Apply[control.ConstructorPattern[any.Type[FT], any.Name[FT]], Generator[PatternContext[FT], any.Expression[FT]], any.Expression[FT]]] = new Understands[PatternContext[FT], Apply[control.ConstructorPattern[any.Type[FT], any.Name[FT]], Generator[PatternContext[FT], any.Expression[FT]], any.Expression[FT]]] {
      override def perform(context: PatternContext[FT], command: Apply[control.ConstructorPattern[any.Type[FT], any.Name[FT]], Generator[PatternContext[FT], any.Expression[FT]], any.Expression[FT]]): (PatternContext[FT], any.Expression[FT]) = {
        val (finalCtxt, argumentPatterns) = command.arguments.foldLeft((context, Seq.empty[any.Expression[FT]])) { case ((ctxt, args), argGen) =>
          val (nextCtxt, nextArg) = Command.runGenerator(argGen, ctxt)
          (nextCtxt, args :+ nextArg)
        }
        val patternApp = factory.constructorPattern(command.functional.tpe, command.functional.constructor, argumentPatterns)
        (finalCtxt, patternApp)
      }
    }
  }
}

object Functional {
  type WithBase[FT <: FinalTypes, FactoryType <: Factory[FT], B <: AnyParadigm.WithFT[FT, FactoryType]] = Functional[FT, FactoryType] { val base: B }
  def apply[FT <: FinalTypes, FactoryType <: Factory[FT], B <: AnyParadigm.WithFT[FT, FactoryType]](_base: B): WithBase[FT, FactoryType, _base.type] = new Functional[FT, FactoryType] {
    val base: _base.type = _base
  }
}