package org.combinators.ep.language.java    /*DD:LD:AI*/

import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.expr.NameExpr
import org.combinators.ep.domain.math._
import org.combinators.ep.domain.{Evolution, OperationDependency}
import org.combinators.ep.generator.LanguageIndependentGenerator
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e4 extends Evolution with JavaGenerator with JUnitTestGenerator with OperationDependency with M0 with M1 with M2 with M3 with M4 {
  self:e0 with e1 with e2 with e3 =>
  val domain:MathDomain

  /** Provides fresh names for temporary list objects. */
  object ListNameGenerator {
    private var nextNumber: Int = 0
    def nextFreshListName(): NameExpr = {
      val nextName = Java(s"tmpList$nextNumber").nameExpression()
      nextNumber += 1
      nextName
    }
  }

  /** E4 Introduces Lists of values. */
  abstract override def toTargetLanguage(ei:domain.ExistsInstance) : CodeBlockWithResultingExpressions = {
    ei.tpe match {
      case tpe: List[_] =>
        ei.inst match {
          case s:Seq[tpe.generic.scalaInstanceType] =>
            val listName = ListNameGenerator.nextFreshListName()
            val initBlock =
              CodeBlockWithResultingExpressions(
                Java(s"${typeConverter(tpe)} $listName = new java.util.ArrayList<>();").statement()
              )(listName)

            s.foldLeft(initBlock) {
              case (block, nextElem) =>
                block.appendDependent { case Seq(constructedList) =>
                  toTargetLanguage(domain.ExistsInstance(tpe.generic)(nextElem)).appendDependent { case Seq(nextElemExpr) =>
                    CodeBlockWithResultingExpressions(
                      Java(s"$constructedList.add($nextElemExpr);").statement()
                    )(constructedList)
                  }
                }
            }

        }
      case _ => super.toTargetLanguage(ei)
    }
  }

  /**
    * Operations can declare dependencies, which leads to #include extras
    */
  override def dependency(op: domain.Operation): scala.List[domain.Operation] = {
    op match {
      case Simplify => scala.List[domain.Operation](Eval)
      case _ => super.dependency(op)
    }
  }

  abstract override def typeConverter(tpe:domain.TypeRep) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case el:List[_] => Java(s"java.util.List<${typeConverter(el.generic)}>").tpe()
      case _ => super.typeConverter(tpe)
    }
  }

  abstract override def logic(exp:domain.DataType, op:domain.Operation): Seq[Statement] = {
    val zero = Java("0.0").expression[Expression]()
    val one = Java("1.0").expression[Expression]()
    val negOne = Java("-1.0").expression[Expression]()
    val zeroResultBlock =
      inst(Lit, zero).appendDependent { case Seq(zeroLit) =>
        CodeBlockWithResultingExpressions(result(zeroLit): _*)()
      }
    val oneResultBlock =
      inst(Lit, one).appendDependent { case Seq(zeroLit) =>
        CodeBlockWithResultingExpressions(result(zeroLit): _*)()
      }
    val negOneResultBlock =
      inst(Lit, negOne).appendDependent { case Seq(zeroLit) =>
        CodeBlockWithResultingExpressions(result(zeroLit): _*)()
      }

    // generate the actual body
    val source = Source(exp,op)

    op match {
        // Simplify only works for solutions that instantiate expression instances
      case Simplify =>

        exp match {
          case Lit =>
            inst(Lit, expression(exp, litValue)).appendDependent{ case Seq(litExp) =>
              CodeBlockWithResultingExpressions(result(litExp):_*)()
            }.block
          case Add =>
            val deltaLeft = deltaChildOp(source, domain.base.left, Eval)
            val deltaRight = deltaChildOp(source, domain.base.right, Eval)

            val dispatchBothResultBlock =
              inst(Add,
                dispatch(expression(exp, domain.base.left), Simplify),
                dispatch(expression(exp, domain.base.right), Simplify)
              ).appendDependent{ case Seq(addResult) =>
                CodeBlockWithResultingExpressions(result(addResult): _*)()
              }
            Java(s"""|double leftVal = ${contextDispatch(source, deltaLeft)};
                     |double rightVal = ${contextDispatch(source, deltaRight)};
                     |if ((leftVal == 0 && rightVal == 0) || (leftVal + rightVal == 0)) {
                     |   ${zeroResultBlock.block.mkString("\n")}
                     |} else if (leftVal == 0) {
                     |   ${result(dispatch(expression(exp, domain.base.right), Simplify)).mkString("\n")}
                     |} else if (rightVal == 0) {
                     |   ${result(dispatch(expression(exp, domain.base.left), Simplify)).mkString("\n")}
                     |} else {
                     |   ${dispatchBothResultBlock.block.mkString("\n")}
                     |}""".stripMargin).statements()
          case Sub =>
            val deltaLeft = deltaChildOp(source, domain.base.left, Eval)
            val deltaRight = deltaChildOp(source, domain.base.right, Eval)
            val dispatchBothResultBlock =
              inst(Sub,
                dispatch(expression(exp, domain.base.left), Simplify),
                dispatch(expression(exp, domain.base.right), Simplify)
              ).appendDependent{ case Seq(addResult) =>
                CodeBlockWithResultingExpressions(result(addResult): _*)()
              }
            Java(s"""|if (${contextDispatch(source, deltaLeft)} == ${contextDispatch(source, deltaRight)}) {
                     |   ${zeroResultBlock.block.mkString("\n")}
                     |} else {
                     |   ${dispatchBothResultBlock.block.mkString("\n")}
                     |}""".stripMargin).statements()
          case Mult =>
            val deltaLeft = deltaChildOp(source, domain.base.left, Eval)
            val deltaRight = deltaChildOp(source, domain.base.right, Eval)
            val dispatchBothResultBlock =
              inst(Mult,
                dispatch(expression(exp, domain.base.left), Simplify),
                dispatch(expression(exp, domain.base.right), Simplify)
              ).appendDependent{ case Seq(addResult) =>
                CodeBlockWithResultingExpressions(result(addResult): _*)()
              }
            Java(s"""|double leftVal = ${contextDispatch(source, deltaLeft)};
                     |double rightVal = ${contextDispatch(source, deltaRight)};
                     |if (leftVal == 0 || rightVal == 0) {
                     |   ${zeroResultBlock.block.mkString("\n")}
                     |} else if (leftVal == 1) {
                     |   ${result(dispatch(expression(exp, domain.base.right), Simplify)).mkString("\n")}
                     |} else if (rightVal == 1) {
                     |   ${result(dispatch(expression(exp, domain.base.left), Simplify)).mkString("\n")}
                     |} else {
                     |   ${dispatchBothResultBlock.block.mkString("\n")}
                     |}
                     |""".stripMargin).statements()
          case Divd =>
            val deltaLeft = deltaChildOp(source, domain.base.left, Eval)
            val deltaRight = deltaChildOp(source, domain.base.right, Eval)
            val dispatchBothResultBlock =
              inst(Divd,
                dispatch(expression(exp, domain.base.left), Simplify),
                dispatch(expression(exp, domain.base.right), Simplify)
              ).appendDependent{ case Seq(addResult) =>
                CodeBlockWithResultingExpressions(result(addResult): _*)()
              }
            Java(s"""|double leftVal = ${contextDispatch(source, deltaLeft)};
                     |double rightVal = ${contextDispatch(source, deltaRight)};
                     |if (leftVal == 0) {
                     |   ${zeroResultBlock.block.mkString("\n")}
                     |} else if (rightVal == 1) {
                     |   ${result(dispatch(expression(exp, domain.base.left), Simplify)).mkString("\n")}
                     |} else if (leftVal == rightVal) {
                     |   ${oneResultBlock.block.mkString("\n")}
                     |} else if (leftVal == -rightVal) {
                     |   ${negOneResultBlock.block.mkString("\n")}
                     |} else {
                     |   ${dispatchBothResultBlock.block.mkString("\n")}
                     |}
                     |""".stripMargin).statements()
            // TODO: Would love to have ability to simplify neg(neg(x)) to just be x. This requires a form
            // of inspection that might not be generalizable...
          case Neg =>
            val deltaInner = deltaChildOp(source, domain.base.inner, Eval)
            val dispatchBothResultBlock =
              inst(Neg, dispatch(expression(exp, domain.base.inner), Simplify))
                .appendDependent{ case Seq(addResult) =>
                  CodeBlockWithResultingExpressions(result(addResult): _*)()
               }
            Java(s"""
                    |if (${contextDispatch(source, deltaInner)} == 0) {
                    |   ${zeroResultBlock.block.mkString("\n")}
                    |} else {
                    |   ${dispatchBothResultBlock.block.mkString("\n")}
                    |}""".stripMargin).statements()
          case _ => super.logic(exp, op)
        }

      case Collect =>
        val emptyList = domain.ExistsInstance(List(Double))(Seq.empty)
        def returnListBlock(collectedLists: Expression*): Seq[Statement] =
          toTargetLanguage(emptyList).appendDependent { case Seq(resultList) =>
            CodeBlockWithResultingExpressions(
              collectedLists.map(col => Java(s"$resultList.addAll($col);").statement()) ++ result(resultList):_*
            )()
          }.block

        val returnList = result(Java("list").expression[Expression]()).mkString("\n")
        exp match {
          case _:domain.Binary =>
            returnListBlock(
              dispatch(expression(exp, domain.base.left), Collect),
              dispatch(expression(exp, domain.base.right), Collect)
            )
          case _:domain.Unary  =>
            returnListBlock(dispatch(expression(exp, domain.base.inner), Collect))
          case _:domain.Atomic =>
            toTargetLanguage(emptyList).appendDependent { case Seq(resultList) =>
              CodeBlockWithResultingExpressions(
                Java(s"$resultList.add(${expression(exp, litValue)});").statement() +: result(resultList):_*
              )()
            }.block
          case _ => super.logic(exp, op)
        }

      case _ => super.logic(exp, op)
    }
  }

  abstract override def testGenerator: Seq[MethodDeclaration] = {
     if (getModel.supports(Simplify)) {
       super.testGenerator ++ testMethod(M4_tests) ++ testMethod(M4_simplify_tests)
    } else {
      super.testGenerator ++ testMethod(M4_tests)
    }
  }
}
