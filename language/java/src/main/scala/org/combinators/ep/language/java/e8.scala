package org.combinators.ep.language.java    /*DD:LD:AI*/

import com.github.javaparser.ast.body.MethodDeclaration
import org.combinators.ep.domain.math._
import org.combinators.ep.domain.Evolution
import org.combinators.ep.generator.OperationDependency
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  */
trait e8 extends Evolution with JavaGenerator with JUnitTestGenerator with OperationDependency with M0 with M2 with M4 with M5 with M6 with M7 with M8 {
  self: e0 with e1 with e2 with e3 with e4 with e5 with e6 with e7 =>

  abstract override def logic(exp:domain.DataType, op:domain.Operation): Seq[Statement] = {
    val source = Source(exp, op)
    op match {

      case Eval =>
        exp match {
          case Power => result(Java(s" Math.pow(${dispatch(expression(exp,domain.base.left), Eval)}, ${dispatch(expression(exp,domain.base.right), Eval)}) ").expression())

          case _ => super.logic(exp, op)
        }

      case PrettyP =>
        exp match {
          case Power => result(Java(s""" "Power(" + ${dispatch(expression(exp,domain.base.left), PrettyP)} + "," + ${dispatch(expression(exp,domain.base.right), PrettyP)}  + ")" """).expression())

          case _ => super.logic(exp, op)
        }

        // this has no simplify possibilities (yet) so just return regular construction.
        // Note, however, that this data type (Sqrt) is defined after the producer method
        // from an earlier level; some approaches may be challenged to make this work
        // (only interpreter for now) but it isn't as simple as moving this logic into the
        // dispatch() method, since that has embedded 'exp' into an expression. The only
        // way to make that work is to unbundle it, and allow dispatch to be defined
        // as dispatch (exp, attname, operation) and then have the default dispatch
        // invoke 'expression(exp, attname)' which it could do since expression is in
        // the languageIndependent API.
      case Simplify =>
        exp match {
          case Power =>
            val zero = Java("0.0").expression[Expression]()
            val one = Java("1.0").expression[Expression]()
            val deltaLeft = dispatchChild(exp, domain.base.left, Eval)
            val deltaRight = dispatchChild(exp, domain.base.right, Eval)

            val zeroResultBlock =
              inst(Lit, zero).appendDependent { case Seq(zeroLit) =>
                CodeBlockWithResultingExpressions(result(zeroLit): _*)()
              }
            val oneResultBlock =
              inst(Lit, one).appendDependent { case Seq(zeroLit) =>
                CodeBlockWithResultingExpressions(result(zeroLit): _*)()
              }

            val dispatchBothResultBlock =
              inst(Power,
                dispatch(expression(exp, domain.base.left), Simplify),
                dispatch(expression(exp, domain.base.right), Simplify)
              ).appendDependent{ case Seq(powerResult) =>
                CodeBlockWithResultingExpressions(result(powerResult): _*)()
              }
            Java(s"""|double leftVal = ${contextDispatch(source, deltaLeft)};
                     |double rightVal = ${contextDispatch(source, deltaRight)};
                     |if (leftVal == 0) {
                     |   ${zeroResultBlock.block.mkString("\n")}
                     |} else if (rightVal == 0) {
                     |   ${oneResultBlock.block.mkString("\n")}
                     |} else if (rightVal == 1) {
                     |   ${result(dispatch(expression(exp, domain.base.left), Simplify)).mkString("\n")}
                     |} else {
                     |   ${dispatchBothResultBlock.block.mkString("\n")}
                     |}""".stripMargin).statements()

          case _ => super.logic(exp, op)
        }

      case domain.AsTree =>
        val atts = subExpressions(exp)
        exp match {
          case Power =>
            val attParams = atts.map(att => att._2.toString + ".astree()").mkString(",")
            val deltaSelf = dispatchSelf(Identifier)
            val rhs = contextDispatch(source, deltaSelf)
            result(Java(s" new tree.Node(java.util.Arrays.asList($attParams), $rhs) ").expression[Expression]())

          case _ => super.logic(exp, op)
        }

      case Copy =>
        exp match {
          case Lit =>
            inst(Lit, expression(exp, litValue)).appendDependent{ case Seq(litExp) =>
              CodeBlockWithResultingExpressions(result(litExp):_*)()
            }.block

          case a:domain.Atomic =>
            val dispatchResultBlock =
              inst(a)
                .appendDependent{ case Seq(copyResult) =>
                  CodeBlockWithResultingExpressions(result(copyResult): _*)()
                }
            Java(dispatchResultBlock.block.mkString("\n")).statements()

          case u:domain.Unary =>
            val dispatchInnerResultBlock =
              inst(u, dispatch(expression(u, domain.base.inner), Copy))
                  .appendDependent{ case Seq(copyResult) =>
                CodeBlockWithResultingExpressions(result(copyResult): _*)()
              }
            Java(dispatchInnerResultBlock.block.mkString("\n")).statements()

          case b:domain.Binary =>
            val dispatchBothResultBlock =
              inst(b,
                dispatch(expression(b, domain.base.left), Copy),
                dispatch(expression(b, domain.base.right), Copy)
              ).appendDependent{ case Seq(copyResult) =>
                CodeBlockWithResultingExpressions(result(copyResult): _*)()
              }
            Java(dispatchBothResultBlock.block.mkString("\n")).statements()
        }

      case _ => super.logic(exp, op)
    }
  }

  abstract override def testGenerator: Seq[MethodDeclaration] = {
    super.testGenerator ++ testMethod(M8_tests)
  }
}
