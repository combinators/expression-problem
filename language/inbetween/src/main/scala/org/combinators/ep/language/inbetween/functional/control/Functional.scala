package org.combinators.ep.language.inbetween.functional.control

import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{Command, Understands, paradigm}
import org.combinators.ep.generator.paradigm.{Apply, Reify, control}
import org.combinators.ep.generator.paradigm.control.{DeclareFunVariable => DFV, Functional => Fun}
import org.combinators.ep.language.inbetween.any
import org.combinators.ep.language.inbetween.any.AnyParadigm

trait Functional[FT <: FinalTypes, FactoryType <: Factory[FT]] extends Fun[any.Method[FT]] {
  val base: AnyParadigm.WithFT[FT, FactoryType]

  import base.factory

  type Ctxt = any.Method[FT]
  override type PatternContext = org.combinators.ep.language.inbetween.functional.control.PatternContext[FT]

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
    implicit val canDeclareVar: Understands[any.Method[FT], DFV[any.Method[FT], any.Name[FT], any.Type[FT], any.Expression[FT], any.Expression[FT]]] =
      new Understands[any.Method[FT], DFV[any.Method[FT], any.Name[FT], any.Type[FT], any.Expression[FT], any.Expression[FT]]] {
        override def perform(context: any.Method[FT], command: DFV[any.Method[FT], any.Name[FT], any.Type[FT], any.Expression[FT], any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
          val (resContext, inExp) = Command.runGenerator(command.inBlk(factory.argumentExpression(command.name)), context)
          (resContext, factory.declareFunVariable(command.name, command.tpe, isRecursive = false, command.initialization, inExp))
        }
    }

    implicit val canDeclareRecVar:  Understands[any.Method[FT], DFV[any.Method[FT], any.Name[FT], any.Type[FT], any.Expression[FT] => Generator[any.Method[FT], any.Expression[FT]], any.Expression[FT]]] =
      new Understands[any.Method[FT], DFV[any.Method[FT], any.Name[FT], any.Type[FT], any.Expression[FT] => Generator[any.Method[FT], any.Expression[FT]], any.Expression[FT]]] {
        override def perform(context: any.Method[FT], command: DFV[any.Method[FT], any.Name[FT], any.Type[FT], any.Expression[FT] => Generator[any.Method[FT], any.Expression[FT]], any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
          val (initContext, initExp) = Command.runGenerator(command.initialization(factory.argumentExpression(command.name)), context)
          val (resContext, inExp) = Command.runGenerator(command.inBlk(factory.argumentExpression(command.name)), initContext)
          (resContext, factory.declareFunVariable(command.name, command.tpe, isRecursive = true, initExp, inExp))
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
          (elseCtxt, factory.funIfThenElse(command.condition, ifBranchExp, elseIfExprs, elseExpr))
        }
      }
    implicit val canPatternMatch: Understands[any.Method[FT], control.PatternMatch[any.Method[FT], PatternContext, any.Expression[FT]]] = new Understands[any.Method[FT], control.PatternMatch[any.Method[FT], PatternContext, any.Expression[FT]]] {
      override def perform(context: any.Method[FT], command: control.PatternMatch[any.Method[FT], PatternContext, any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
        val (finalCtxt, cases) = command.options.foldLeft((context, Seq.empty[(any.Expression[FT], any.Expression[FT])])) {
          case ((ctxt, cases), option) =>
            val (patternCtxt, patternExp) = Command.runGenerator[PatternContext, any.Expression[FT]](option._1, factory.convert(ctxt).emptyPatternCtxt)
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
    implicit val canPatternVariable: Understands[PatternContext, control.PatternVariable[any.Name[FT], any.Expression[FT]]] = new Understands[PatternContext, control.PatternVariable[any.Name[FT], any.Expression[FT]]] {
      override def perform(context: PatternContext, command: control.PatternVariable[any.Name[FT], any.Expression[FT]]): (PatternContext, any.Expression[FT]) = {
        (context.copy(context.variables :+ command.name), factory.patternVariable(command.name))
      }
    }
    implicit def canReifyInPattern[T]: Understands[PatternContext, Reify[T, any.Expression[FT]]] = new Understands[PatternContext, Reify[T, any.Expression[FT]]] {
      def perform(context: PatternContext, command: Reify[T, any.Expression[FT]]): (PatternContext, any.Expression[FT]) = {
        (context, context.reify(command.tpe, command.value))
      }
    }
    
    implicit val canApplyConstructorPattern: Understands[PatternContext, Apply[control.ConstructorPattern[any.Type[FT], any.Name[FT]], Generator[PatternContext, any.Expression[FT]], any.Expression[FT]]] = new Understands[PatternContext, Apply[control.ConstructorPattern[any.Type[FT], any.Name[FT]], Generator[PatternContext, any.Expression[FT]], any.Expression[FT]]] {
      override def perform(context: PatternContext, command: Apply[control.ConstructorPattern[any.Type[FT], any.Name[FT]], Generator[PatternContext, any.Expression[FT]], any.Expression[FT]]): (PatternContext, any.Expression[FT]) = {
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