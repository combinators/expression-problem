package org.combinators.ep.language.inbetween.ffi

trait EqualsAST extends OperatorExpressionOpsAST  {
  object equalsOp {
    trait Equals extends any.Expression {
      def tpe: any.Type
      def left: any.Expression
      def right: any.Expression

      def copy(
        tpe: any.Type,
        left: any.Expression = left,
        right: any.Expression = right
      ): Equals = equalsOpFactory.equals(tpe, left, right)
    }

    trait Factory {
      def equals(tpe: any.Type, left: any.Expression, right: any.Expression): Equals
    }
  }
  val equalsOpFactory: equalsOp.Factory
}