package org.combinators.ep.language.scala.ast.ffi

import org.combinators.ep.language.inbetween.ffi.{AssertionsAST => InbetweenAssertionsAST}
import org.combinators.ep.language.scala.ast.BaseAST
import org.combinators.ep.language.scala.ast.ffi.OperatorExpressionsAST

trait AssertionsAST extends InbetweenAssertionsAST { self: OperatorExpressionsAST & BaseAST =>
  object scalaAssertions {
    object assertionOpsOverride {
      trait AssertTrueOp extends assertionOps.AssertTrueOp with scalaOperatorExpressions.operatorExpressionsOverrides.Operator {
        import factory.*
        def operator: String = "assert "
        def toScala(operands: any.Expression*): String = s"assert (${operands.head.toScala})"
      }
      
      trait Factory extends assertionOps.Factory {}
    }
  }
  val assertionOpsFactory: scalaAssertions.assertionOpsOverride.Factory
}

trait FinalAssertionsAST extends AssertionsAST { self: FinalOperatorExpressionsAST & BaseAST =>
  object finalAssertionsFactoryTypes {
    trait AssertionsFactory extends scalaAssertions.assertionOpsOverride.Factory {
      def assertTrueOp(): scalaAssertions.assertionOpsOverride.AssertTrueOp = {
        case class AssertTrueOp() extends scalaAssertions.assertionOpsOverride.AssertTrueOp {

          def getSelfOperator: operatorExpressionsFinalTypes.Operator = this
        }

        AssertTrueOp()
      }
    }
  }

  val assertionOpsFactory: finalAssertionsFactoryTypes.AssertionsFactory = new finalAssertionsFactoryTypes.AssertionsFactory {}
}