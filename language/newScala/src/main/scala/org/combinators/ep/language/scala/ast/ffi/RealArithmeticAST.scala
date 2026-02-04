package org.combinators.ep.language.scala.ast.ffi

import org.combinators.ep.language.inbetween.ffi.RealArithmeticAST as InbetweenRealArithmeticAST
import org.combinators.ep.language.scala.ast.{BaseAST, FinalBaseAST}
import org.combinators.ep.language.scala.ast.ffi.OperatorExpressionsAST

trait RealArithmeticOpsAST extends InbetweenRealArithmeticAST { self: OperatorExpressionsAST & BaseAST =>
  object scalaRealArithmeticOps {
    object realArithmeticOpsOverride {

      trait SqrtOp
        extends realArithmeticOps.SqrtOp
        with scalaOperatorExpressions.operatorExpressionsOverrides.Operator
        with scalaOperatorExpressions.MathFunctionOperator {
        override def operator: String = "sqrt"
      }

      trait PowOp extends realArithmeticOps.PowOp
        with scalaOperatorExpressions.operatorExpressionsOverrides.Operator
        with scalaOperatorExpressions.MathFunctionOperator {
        override def operator: String = "pow"
      }

      trait LogOp extends realArithmeticOps.LogOp
        with scalaOperatorExpressions.operatorExpressionsOverrides.Operator
        with scalaOperatorExpressions.MathFunctionOperator {
        import factory.*
        override def operator: String = "log"
        override def toScala(operands: any.Expression*): String = {
          s"(Math.$operator(${operands(0).toScala})/Math.$operator(${operands(1).toScala}))"
        }
      }

      trait SinOp extends realArithmeticOps.SinOp
        with scalaOperatorExpressions.operatorExpressionsOverrides.Operator
        with scalaOperatorExpressions.MathFunctionOperator {
        override def operator: String = "sin"
      }

      trait CosOp extends realArithmeticOps.CosOp
        with scalaOperatorExpressions.operatorExpressionsOverrides.Operator
        with scalaOperatorExpressions.MathFunctionOperator {
        override def operator: String = "cos"
      }

      trait AbsOp extends realArithmeticOps.AbsOp
        with scalaOperatorExpressions.operatorExpressionsOverrides.Operator
        with scalaOperatorExpressions.MathFunctionOperator {
        override def operator: String = "abs"
      }

      trait FloorOp extends realArithmeticOps.FloorOp
        with scalaOperatorExpressions.operatorExpressionsOverrides.Operator
        with scalaOperatorExpressions.MathFunctionOperator {
        override def operator: String = "floor"
      }

      trait EulersNumber extends realArithmeticOps.EulersNumber with scalaBase.anyOverrides.Expression {
        override def toScala: String = "Math.E"

        override def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): EulersNumber =
          this
      }

      trait Pi extends realArithmeticOps.Pi with scalaBase.anyOverrides.Expression{
        override def toScala: String = "Math.PI"

        override def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): Pi =
          this
      }

      trait Factory extends realArithmeticOps.Factory {}
    }
  }
  val realArithmeticOpsFactory: scalaRealArithmeticOps.realArithmeticOpsOverride.Factory
}

trait FinalRealArithmeticOpsAST extends RealArithmeticOpsAST { self: FinalOperatorExpressionsAST & FinalBaseAST =>
  object finalRealArithmeticFactoryTypes {
    trait FinalRealArithmeticFactory extends scalaRealArithmeticOps.realArithmeticOpsOverride.Factory {
      def sqrtOp(): realArithmeticOps.SqrtOp = {
        case class SqrtOp() extends scalaRealArithmeticOps.realArithmeticOpsOverride.SqrtOp
          with finalOperatorExpressions.operatorExpressionsOverrides.Operator {}
        SqrtOp()
      }
      def powOp(): realArithmeticOps.PowOp = {
        case class PowOp() extends scalaRealArithmeticOps.realArithmeticOpsOverride.PowOp
          with finalOperatorExpressions.operatorExpressionsOverrides.Operator {}
        PowOp()
      }
      def logOp(): realArithmeticOps.LogOp = {
        case class LogOp() extends scalaRealArithmeticOps.realArithmeticOpsOverride.LogOp
          with finalOperatorExpressions.operatorExpressionsOverrides.Operator {}
        LogOp()
      }
      def sinOp(): realArithmeticOps.SinOp = {
        case class SinOp() extends scalaRealArithmeticOps.realArithmeticOpsOverride.SinOp
          with finalOperatorExpressions.operatorExpressionsOverrides.Operator {}
        SinOp()
      }
      def cosOp(): realArithmeticOps.CosOp = {
        case class CosOp() extends scalaRealArithmeticOps.realArithmeticOpsOverride.CosOp
          with finalOperatorExpressions.operatorExpressionsOverrides.Operator {}
        CosOp()
      }
      def absOp(): realArithmeticOps.AbsOp = {
        case class AbsOp() extends scalaRealArithmeticOps.realArithmeticOpsOverride.AbsOp
          with finalOperatorExpressions.operatorExpressionsOverrides.Operator {}
        AbsOp()
      }
      def floorOp(): realArithmeticOps.FloorOp= {
        case class FloorOp() extends scalaRealArithmeticOps.realArithmeticOpsOverride.FloorOp
          with finalOperatorExpressions.operatorExpressionsOverrides.Operator {}
        FloorOp()
      }
      def pi(): realArithmeticOps.Pi = {
        case class Pi() extends scalaRealArithmeticOps.realArithmeticOpsOverride.Pi
          with finalBaseAST.anyOverrides.FinalExpression {}
        Pi()
      }
      def eulersNumber(): realArithmeticOps.EulersNumber = {
        case class EulersNumber() extends scalaRealArithmeticOps.realArithmeticOpsOverride.EulersNumber
          with finalBaseAST.anyOverrides.FinalExpression {}
        EulersNumber()
      }
    }
  }
  
  val realArithmeticOpsFactory: finalRealArithmeticFactoryTypes.FinalRealArithmeticFactory = new finalRealArithmeticFactoryTypes.FinalRealArithmeticFactory {}
}
