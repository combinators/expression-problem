package org.combinators.ep.language.scala.ast.ffi

import org.combinators.ep.language.inbetween.ffi.{ConsoleAST => InbetweenConsoleAST}
import org.combinators.ep.language.scala.ast.BaseAST
import org.combinators.ep.language.scala.ast.ffi.OperatorExpressionsAST

trait ConsoleAST extends InbetweenConsoleAST { self: OperatorExpressionsAST & BaseAST =>
  object scalaConsole {
    object consoleOpsOverride {
      trait ConsolePrintOp extends consoleOps.ConsolePrintOp with scalaOperatorExpressions.operatorExpressionsOverrides.Operator {
        import factory.*
        def operator: String = "println "
        def toScala(operands: any.Expression*): String = s"println (${operands.head.toScala})"
      }
      
      trait Factory extends consoleOps.Factory {}
    }
  }
  val consoleOpsFactory: scalaConsole.consoleOpsOverride.Factory
}

trait FinalConsoleAST extends ConsoleAST { self: FinalOperatorExpressionsAST & BaseAST =>
  object finalConsoleFactoryTypes {
    trait ConsoleFactory extends scalaConsole.consoleOpsOverride.Factory {
      def consolePrintOp(): scalaConsole.consoleOpsOverride.ConsolePrintOp = {
        case class ConsolePrintOp() extends scalaConsole.consoleOpsOverride.ConsolePrintOp {

          def getSelfOperator: operatorExpressionsFinalTypes.Operator = this
        }

        ConsolePrintOp()
      }
    }
  }

  val consoleOpsFactory: finalConsoleFactoryTypes.ConsoleFactory = new finalConsoleFactoryTypes.ConsoleFactory {}
}