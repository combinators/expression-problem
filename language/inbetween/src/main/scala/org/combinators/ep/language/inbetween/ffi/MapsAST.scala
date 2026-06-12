package org.combinators.ep.language.inbetween.ffi    /*DI:LI:AI*/

import org.combinators.ep.language.inbetween.polymorphism.ParametricPolymorphismAST

trait MapsAST extends OperatorExpressionOpsAST with ParametricPolymorphismAST {
  object mapsOps {
    trait FinalTypes {
      type Map <: mapsOps.Map
      type CreateMapExpression <: mapsOps.CreateMap
    }

    trait Map extends any.Type {
      def getSelfMapType: mapsOpsFinalTypes.Map

      def copy() : Map = mapsOpsFactory.map()
    }

    trait CreateMap extends any.Expression {
      def getSelfCreateMapExpression: mapsOpsFinalTypes.CreateMapExpression

      def keyType:any.Type
      def elementType: any.Type
      
      def initialKeyValuePairs: Seq[(any.Expression,any.Expression)]

      def copy(keyType: any.Type = this.keyType, 
               elementType: any.Type = this.elementType,
               initialKeyValuePairs: Seq[(any.Expression, any.Expression)] = this.initialKeyValuePairs): CreateMap =
        mapsOpsFactory.createMap(keyType, elementType, initialKeyValuePairs)
    }
    
    trait ContainsKeyOp extends operatorExpressions.Operator
    trait GetOp extends operatorExpressions.Operator
    trait PutOp extends operatorExpressions.Operator {
      def keyType: any.Type
      def elementType: any.Type
      
      def copy(keyType: any.Type = this.keyType, elementType: any.Type = this.elementType): PutOp = mapsOpsFactory.putOp(keyType = keyType, elementType = elementType)
    }

    trait Factory {
      def createMap(keyType: any.Type,
                    elementType: any.Type,
                    keyValuePairs: Seq[(any.Expression,any.Expression)]): CreateMap

      def map(): Map
      def map(keyType: any.Type, elementType: any.Type): any.Type = {
        polymorphismFactory.typeApplication(map(), Seq(keyType, elementType))
      }
      
      def containsKeyOp(): ContainsKeyOp
      def getOp(): GetOp
      def putOp(keyType:any.Type, elementType: any.Type): PutOp

      def containsKey(map: any.Expression, key: any.Expression): operatorExpressions.BinaryExpression =
        operatorExpressionsFactory.binaryExpression(containsKeyOp(), map, key)

      def get(map: any.Expression, key: any.Expression, defaultValue: any.Expression): operatorExpressions.TernaryExpression =
        operatorExpressionsFactory.ternaryExpression(getOp(), map, key, defaultValue)

      def put(map: any.Expression, keyType: any.Type, elementType: any.Type, key: any.Expression, value: any.Expression): operatorExpressions.TernaryExpression =
        operatorExpressionsFactory.ternaryExpression(putOp(keyType, elementType), map, key, value)

      implicit def convert(other: CreateMap): mapsOpsFinalTypes.CreateMapExpression = other.getSelfCreateMapExpression
      implicit def convert(other: Map): mapsOpsFinalTypes.Map = other.getSelfMapType
    }
  }

  val mapsOpsFinalTypes: mapsOps.FinalTypes
  val mapsOpsFactory: mapsOps.Factory
}
