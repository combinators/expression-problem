package org.combinators.ep.language.inbetween.ffi    /*DI:LI:AI*/

trait ConsoleAST extends OperatorExpressionOpsAST  {
  object consoleOps {
    trait ConsolePrintOp extends operatorExpressions.Operator

    trait Factory {
      def consolePrintOp(): ConsolePrintOp

      def consolePrintOp(exp: any.Expression): operatorExpressions.UnaryExpression =
        operatorExpressionsFactory.unaryExpression(consolePrintOp(), exp)
    }
  }
  val consoleOpsFactory: consoleOps.Factory
}
