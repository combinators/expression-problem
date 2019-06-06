package org.combinators.ep.language.java    /*DD:LD:AI*/

import com.github.javaparser.ast.body.MethodDeclaration
import org.combinators.ep.domain.{Evolution, OperationDependency}
import org.combinators.ep.domain.math.{M0, M1, M2, P1}
import org.combinators.templating.twirl.Java

/**
  * Independent branch to work with paper.
  */
trait p1 extends Evolution with JavaGenerator with JUnitTestGenerator with OperationDependency with M0 with M1 with M2 with P1 {
  self: e0 with e1 with e2  =>

  abstract override def typeConverter(tpe:domain.TypeRep) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case Int => Java(s"Integer").tpe()
      case _ => super.typeConverter(tpe)
    }
  }

  /** p1 has Amortized */
  abstract override def toTargetLanguage(ei:domain.ExistsInstance) : CodeBlockWithResultingExpressions = {
    ei.inst match {
      case ai:AmortizedInst =>
        toTargetLanguage(ai.P)
        .appendIndependent(toTargetLanguage(ai.r))
        .appendIndependent(toTargetLanguage(ai.n))
        .appendDependent(innerResults => inst(ai.e, innerResults: _*))

      case _ => super.toTargetLanguage(ei)
    }
  }

  /**
    * Operations can declare dependencies, which leads to #include extras
    */
  override def dependency(op: domain.Operation): scala.List[domain.Operation] = {
    op match {
      case Output => scala.List[domain.Operation](PrettyP)
      case _ => super.dependency(op)
    }
  }

   abstract override def logic(exp:domain.DataType, op:domain.Operation): Seq[Statement] = {
    // generate the actual body
    op match {
      case ParamHeight =>
        val heightPlusOne:Expression = Java(s"${independent.height} + 1").expression()
        exp match {
          case _:domain.Binary => result(Java(s"Math.max(${dispatch(expression(exp, domain.base.left), ParamHeight, heightPlusOne)},${dispatch(expression(exp, domain.base.right), ParamHeight, heightPlusOne)}) ").expression())
          case _:domain.Unary => result(Java(s"${dispatch(expression(exp, domain.base.inner), ParamHeight, heightPlusOne)}").expression())
          case _:domain.Atomic => result(Java(s" ${independent.height}").expression())

          case _ => super.logic(exp, op)
        }

      case PrettyP =>
        exp match {
          case Pi => result (Java ("\"Pi\"").expression () )
          case Rnd => result (Java ("\"Rnd\"").expression () )
          case Amortized => {
            val P = dispatch(expression(exp, independent.P), PrettyP)
            val r = dispatch(expression(exp, independent.r), PrettyP)
            val n = dispatch(expression(exp, independent.n), PrettyP)
            result(Java(s"""  "Amortized(P=" + $P +", r=" + $r + ", n=" + $n + ")" """).expression())
          }
          case _ => super.logic(exp, op)
        }

        // Finds all Literals in expression whose value is
      case CountBetween =>
        val low = Java(independent.low).expression[Expression]()
        val high = Java(independent.high).expression[Expression]()
        exp match {
          case Lit =>
            Java(
              s"""|double _litEval = ${expression(exp, litValue)};
                  |if ((_litEval >= ${independent.low}) && (_litEval <= ${independent.high})) { return 1; } else { return 0; }
                  |""".stripMargin).statements()

           case Pi =>
            Java(s"if ((Math.PI >= ${independent.low}) && (Math.PI <= ${independent.high})) { return 1; } else { return 0; }").statements()

          case at:domain.DataType =>
            if (at.attributes.nonEmpty) {
              val expr = at.attributes.map(att => dispatch(expression(exp, att), CountBetween, low, high)).mkString(" + ")
              result(Java(expr).expression[Expression]())
            } else {
              result(Java("0").expression[Expression]())
            }

            // not sure how Rnd plays out.

          // all else simply are not involved
          case _ => result(Java("0").expression[Expression]())
        }

        // FIX ME
      case Output =>
        val source = Source(exp,op)
        val delta = deltaSelfOp(PrettyP)
        Java(s""" System.out.println(${contextDispatch(source, delta)}); """).statements()

      case Eval =>
        exp match {
            case Pi => result(Java(s"${Math.PI}").expression())

            // wasteful but works
            case Rnd => result(Java(s"new java.util.Random().nextDouble()").expression())

            case Amortized =>
              val source = Source(exp,op)

              val deltaP = deltaChildOp(exp, independent.P, Eval)
              val deltaR = deltaChildOp(exp, independent.r, Eval)
              val deltaN = deltaChildOp(exp, independent.n, Eval)

              val comp = Java("P*R*Math.pow(1+R,N)/(Math.pow(1+R,N)-1)").expression[Expression]()
              val retval = Java("val_").expression[Expression]()
              val str = s"""|double P = ${contextDispatch(source, deltaP)};
                            |double R = (${contextDispatch(source, deltaR)}/12.0);   // yearly interest rate into monthly;
                            |double N = ${contextDispatch(source, deltaN)};          // period in months
                            |double val_ = $comp;
                            |// round to pennies accuracy.
                            |val_ = Math.round(val_*100)/100.0;
                            |${result(retval).mkString("\n")}
                            |""".stripMargin
              Java(str).statements()

          case _ => super.logic(exp, op)
        }

      case _ => super.logic(exp, op)
    }
  }

  // TODO: CONVERT TO TEST CASES...
  abstract override def testGenerator: Seq[MethodDeclaration] = {
    super.testGenerator ++ testMethod(P1_tests)
  }
}
