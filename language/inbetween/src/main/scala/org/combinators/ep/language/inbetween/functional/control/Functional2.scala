package org.combinators.ep.language.inbetween.functional.control

import org.combinators.cogen.paradigm.{Apply, IfThenElse, Reify, control}
import org.combinators.cogen.paradigm.control.{ConstructorPattern, DeclareFunVariable, Lambda, PatternMatch, PatternVariable}
import org.combinators.cogen.Command.Generator
import org.combinators.cogen.{Command, Understands, paradigm}
import org.combinators.cogen.paradigm.control
import org.combinators.cogen.paradigm.control.{DeclareFunVariable as DFV, Functional as Fun}
import org.combinators.ep.language.inbetween.any
import org.combinators.ep.language.inbetween.any.{AnyParadigm, AnyParadigm2}

trait Functional2(val _base: AnyParadigm2.WithAST[FunctionalControlAST]) {
  trait FunctionalInMethods extends control.Functional[_base.ast.any.Method] {
    override val base: _base.type = _base

    import base.ast.any
    import base.ast.factory
    import base.ast.functionalControlFactory

    type Ctxt = any.Method
    override type PatternContext = base.ast.funcontrol.PatternContext

    val lambdaCapabilities: LambdaCapabilities = new LambdaCapabilities {
      implicit val canLambda: Understands[any.Method, Lambda[any.Name, any.Type, any.Method, any.Expression]] = new Understands[any.Method, Lambda[any.Name, any.Type, any.Method, any.Expression]] {
        override def perform(context: any.Method, command: Lambda[any.Name, any.Type, any.Method, any.Expression]): (any.Method, any.Expression) = {
          val args = command.variables.map { case (pname, _tpe) => (pname, factory.argumentExpression(pname)) }.toMap
          val (updatedCtxt, body) = Command.runGenerator(command.body(args), context)
          (updatedCtxt, functionalControlFactory.lambda(command.variables, body))
        }
      }
    }

    val functionalCapabilities: FunctionalCapabilities = new FunctionalCapabilities {
      implicit val canDeclareVar: Understands[any.Method, DeclareFunVariable[any.Method, any.Name, any.Type, any.Expression, any.Expression]] =
        new Understands[any.Method, DeclareFunVariable[any.Method, any.Name, any.Type, any.Expression, any.Expression]] {
          override def perform(context: any.Method, command: DeclareFunVariable[any.Method, any.Name, any.Type, any.Expression, any.Expression]): (any.Method, any.Expression) = {
            val (resContext, inExp) = Command.runGenerator(command.inBlk(factory.argumentExpression(command.name)), context)
            (resContext, functionalControlFactory.declareFunVariable(command.name, command.tpe, isRecursive = false, command.initialization, inExp))
          }
        }

      implicit val canDeclareRecVar: Understands[any.Method, DeclareFunVariable[any.Method, any.Name, any.Type, any.Expression => Generator[any.Method, any.Expression], any.Expression]] =
        new Understands[any.Method, DeclareFunVariable[any.Method, any.Name, any.Type, any.Expression => Generator[any.Method, any.Expression], any.Expression]] {
          override def perform(context: any.Method, command: DeclareFunVariable[any.Method, any.Name, any.Type, any.Expression => Generator[any.Method, any.Expression], any.Expression]): (any.Method, any.Expression) = {
            val (initContext, initExp) = Command.runGenerator(command.initialization(factory.argumentExpression(command.name)), context)
            val (resContext, inExp) = Command.runGenerator(command.inBlk(factory.argumentExpression(command.name)), initContext)
            (resContext, functionalControlFactory.declareFunVariable(command.name, command.tpe, isRecursive = true, initExp, inExp))
          }
        }

      implicit val canIfThenElse: Understands[any.Method, IfThenElse[any.Expression, Generator[any.Method, any.Expression], Generator[any.Method, any.Expression], any.Expression]] =
        new Understands[any.Method, IfThenElse[any.Expression, Generator[any.Method, any.Expression], Generator[any.Method, any.Expression], any.Expression]] {
          override def perform(context: any.Method, command: IfThenElse[any.Expression, Generator[any.Method, any.Expression], Generator[any.Method, any.Expression], any.Expression]): (any.Method, any.Expression) = {
            val (startingCtxt, ifBranchExp) = Command.runGenerator[any.Method, any.Expression](command.ifBranch, context)
            val (elseIfCtxt, elseIfExprs) = command.elseIfBranches.foldLeft[(any.Method, Seq[(any.Expression, any.Expression)])]((startingCtxt, Seq.empty)) { case ((ctxt, exprs), elseIfBranch) =>
              val (nextCtxt, elseIfExpr) = Command.runGenerator[any.Method, any.Expression](elseIfBranch._2, ctxt)
              (nextCtxt, exprs :+ (elseIfBranch._1, elseIfExpr))
            }
            val (elseCtxt, elseExpr) = Command.runGenerator[any.Method, any.Expression](command.elseBranch, elseIfCtxt)
            (elseCtxt, functionalControlFactory.funIfThenElse(command.condition, ifBranchExp, elseIfExprs, elseExpr))
          }
        }
      implicit val canPatternMatch: Understands[any.Method, PatternMatch[any.Method, PatternContext, any.Expression]] = new Understands[any.Method, PatternMatch[any.Method, PatternContext, any.Expression]] {
        override def perform(context: any.Method, command: PatternMatch[any.Method, PatternContext, any.Expression]): (any.Method, any.Expression) = {
          val (finalCtxt, cases) = command.options.foldLeft((context, Seq.empty[(any.Expression, any.Expression)])) {
            case ((ctxt, cases), option) =>
              val (patternCtxt, patternExp) = Command.runGenerator[PatternContext, any.Expression](option._1, factory.convert(ctxt).emptyPatternCtxt)
              val patternVariables = patternCtxt.variables.distinct.map(name => factory.argumentExpression(name))
              val branchGen = option._2(patternVariables)
              val (nextCtxt, branchExp) = Command.runGenerator(branchGen, ctxt)
              (nextCtxt, cases :+ (patternExp, branchExp))
          }
          (finalCtxt, functionalControlFactory.patternMatch(command.onValue, cases))
        }
      }
    }

    override val patternCapabilities: PatternCapabilities = new PatternCapabilities {
      implicit val canPatternVariable: Understands[PatternContext, PatternVariable[any.Name, any.Expression]] = new Understands[PatternContext, PatternVariable[any.Name, any.Expression]] {
        override def perform(context: PatternContext, command: PatternVariable[any.Name, any.Expression]): (PatternContext, any.Expression) = {
          (context.copy(context.variables :+ command.name), functionalControlFactory.patternVariable(command.name))
        }
      }
      implicit def canReifyInPattern[T]: Understands[PatternContext, Reify[T, any.Expression]] = new Understands[PatternContext, Reify[T, any.Expression]] {
        def perform(context: PatternContext, command: Reify[T, any.Expression]): (PatternContext, any.Expression) = {
          (context, context.reify(command.tpe, command.value))
        }
      }

      implicit val canApplyConstructorPattern: Understands[PatternContext, Apply[ConstructorPattern[any.Type, any.Name], Generator[PatternContext, any.Expression], any.Expression]] = new Understands[PatternContext, Apply[ConstructorPattern[any.Type, any.Name], Generator[PatternContext, any.Expression], any.Expression]] {
        override def perform(context: PatternContext, command: Apply[ConstructorPattern[any.Type, any.Name], Generator[PatternContext, any.Expression], any.Expression]): (PatternContext, any.Expression) = {
          val (finalCtxt, argumentPatterns) = command.arguments.foldLeft((context, Seq.empty[any.Expression])) { case ((ctxt, args), argGen) =>
            val (nextCtxt, nextArg) = Command.runGenerator(argGen, ctxt)
            (nextCtxt, args :+ nextArg)
          }
          val patternApp = functionalControlFactory.constructorPattern(command.functional.tpe, command.functional.constructor, argumentPatterns)
          (finalCtxt, patternApp)
        }
      }
    }
  }
  val functionalControlInMethods: FunctionalInMethods = new FunctionalInMethods {}
}

object Functional2 {
  type WithBase[AST <: FunctionalControlAST, B <: AnyParadigm2.WithAST[AST]] = Functional2 {val _base: B}

  trait WB[AST <: FunctionalControlAST, B <: AnyParadigm2.WithAST[AST]](override val _base: B) extends Functional2 {}

  def apply[AST <: FunctionalControlAST, B <: AnyParadigm2.WithAST[AST]](_base: B): WithBase[AST, _base.type] = new WB[AST, _base.type](_base) with Functional2(_base) {}
}