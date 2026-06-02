package org.combinators.ep.language.inbetween.ffi    /*DI:LI:AI*/

import org.combinators.ep.language.inbetween.polymorphism.ParametricPolymorphismAST

trait MapsAST extends OperatorExpressionOpsAST with ParametricPolymorphismAST {
  object mapsOps {
    trait CreateMap extends any.Type
    trait ContainsKeyOp extends operatorExpressions.Operator
    trait GetOp extends operatorExpressions.Operator
    trait PutOp extends operatorExpressions.Operator

    trait Factory {
      def createMap(): CreateMap

      def createMap(tpe: any.Type, elems: Seq[any.Expression]): any.ApplyExpression =
        factory.applyExpression(polymorphismFactory.typeReferenceExpression(polymorphismFactory.typeApplication(createMap(), Seq(tpe))),
          elems)

      def containsKeyOp(): ContainsKeyOp
      def getOp(): GetOp
      def putOp(): PutOp

      def containsKey(key: any.Expression, map: any.Expression): operatorExpressions.BinaryExpression =
        operatorExpressionsFactory.binaryExpression(containsKeyOp(), key, map)

      def get(key: any.Expression, map: any.Expression): operatorExpressions.BinaryExpression =
        operatorExpressionsFactory.binaryExpression(getOp(), key, map)

      def put(key: any.Expression, map: any.Expression, value: any.Expression): operatorExpressions.TernaryExpression =
        operatorExpressionsFactory.ternaryExpression(putOp(), key, map, value)
    }
  }

  val mapsOpsFactory: mapsOps.Factory
}