package org.combinators.ep.language.java   /*DD:LD:AI*/

import com.github.javaparser.ast.body.MethodDeclaration
import org.combinators.ep.domain.math._
import org.combinators.ep.domain.{Evolution, ModelDomain, OperationDependency}
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  */
trait e7 extends Evolution with JavaGenerator with JUnitTestGenerator with OperationDependency with M0 with M2 with M4 with M5 with M6 with M7 {
  self: e0 with e1 with e2 with e3 with e4 with e5 with e6 =>
  val domain:MathDomain with ModelDomain

  abstract override def logic(exp:domain.DataType, op:domain.Operation): Seq[Statement] = {
    val source = Source(exp, op)
    op match {

      case Eval =>
        exp match {
          case Sqrt => result(Java(s" Math.sqrt(${dispatch(expression(exp,domain.base.inner), Eval)}) ").expression[Expression]())

          case _ => super.logic(exp, op)
        }

      case PrettyP =>
        exp match {
          case Sqrt => result(Java(s""" "Sqrt(" + ${dispatch(expression(exp,domain.base.inner), PrettyP)} + ")" """).expression[Expression]())

          case _ => super.logic(exp, op)
        }

      case Simplify =>
        exp match {
          case Sqrt =>
            val deltaInner = deltaChildOp(exp, domain.base.inner, Eval)
            val dispatchBothResultBlock =
              inst(Sqrt, dispatch(expression(exp, domain.base.inner), Simplify))
                .appendDependent{ case Seq(addResult) =>
                  CodeBlockWithResultingExpressions(result(addResult): _*)()
                }
            Java(dispatchBothResultBlock.block.mkString("\n")).statements()

          case _ => super.logic(exp, op)
        }

      case domain.AsTree =>
        val atts = subExpressions(exp)
        exp match {
          case Sqrt =>
            val attParams = atts.map(att => att._2.toString + ".astree()").mkString(",")
            val deltaSelf = deltaSelfOp(Identifier)
            val rhs = contextDispatch(source, deltaSelf)
            result(Java(s" new tree.Node(java.util.Arrays.asList($attParams), $rhs) ").expression[Expression]())

          case _ => super.logic(exp, op)
        }

      case Find =>
        val target = Java(m7_extensions.target).expression[Expression]()
        exp match {
          case Lit =>
            Java(
              s"""|double _litEval = ${expression(exp, litValue)};
                  |if (_litEval == $target){ return 1; } else { return 0; }
                  |""".stripMargin).statements()

          case at:domain.DataType =>
            val typeAtts = at.attributes.filter(att => att.tpe == domain.baseTypeRep)
            if (typeAtts.nonEmpty) {
              val expr = at.attributes.map(att => dispatch(expression(exp, att), Find, target)).mkString(" + ")
              result(Java(expr).expression[Expression]())
            } else {
              result(Java("0").expression[Expression]())
            }

          // all else simply are not involved
          case _ => result(Java("0").expression[Expression]())
        }

      case _ => super.logic(exp, op)
    }
  }

  abstract override def testGenerator: Seq[MethodDeclaration] = {
    super.testGenerator ++ testMethod(M7_tests)
  }
}
