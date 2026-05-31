package org.combinators.ep.language.scala.ast.ffi

import org.combinators.ep.language.inbetween.ffi.{ExceptionsAST => InbetweenExceptionsAST}
import org.combinators.ep.language.scala.ast.BaseAST
import org.combinators.ep.language.scala.ast.ffi.OperatorExpressionsAST

trait ExceptionsAST extends InbetweenExceptionsAST { self: OperatorExpressionsAST & BaseAST =>
  object scalaExceptions {
    object exceptionsOpsOverride {
      trait ExceptionsRaiseOp extends exceptionsOps.RaiseOp with scalaOperatorExpressions.operatorExpressionsOverrides.Operator {
        import factory.*
        def operator: String = "raise "
        def toScala(operands: any.Expression*): String = s"raise ${operands.head.toScala}"
      }
      
      trait Factory extends exceptionsOps.Factory {}
    }
  }
  val exceptionsOpsFactory: scalaExceptions.exceptionsOpsOverride.Factory
}

trait FinalExceptionsAST extends ExceptionsAST { self: FinalOperatorExpressionsAST & BaseAST =>
  object finalExceptionsFactoryTypes {
    trait ExceptionsFactory extends scalaExceptions.exceptionsOpsOverride.Factory {
      def raiseOp(): scalaExceptions.exceptionsOpsOverride.ExceptionsRaiseOp = {
        case class Raise() extends scalaExceptions.exceptionsOpsOverride.ExceptionsRaiseOp {

          def getSelfOperator: operatorExpressionsFinalTypes.Operator = this
        }

        Raise()
      }
    }
  }

  val exceptionsOpsFactory: finalExceptionsFactoryTypes.ExceptionsFactory = new finalExceptionsFactoryTypes.ExceptionsFactory {}
}