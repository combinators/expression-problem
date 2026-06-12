package org.combinators.ep.language.scala.ast.ffi     /*DI:LD:AI*/

import org.combinators.ep.language.inbetween.ffi.MapsAST as InbetweenMapsAST
import org.combinators.ep.language.scala.ast.{BaseAST, FinalBaseAST}
import org.combinators.cogen.TypeRep

trait MapsAST extends InbetweenMapsAST { self: OperatorExpressionsAST & BaseAST =>
  object scalaMapsOps {
    object mapsOpsOverride {
      trait FinalTypes extends mapsOps.FinalTypes {
        type Map <: mapsOpsOverride.Map
        type CreateMapExpression <: mapsOpsOverride.CreateMap
      }

      trait Map extends mapsOps.Map with scalaBase.anyOverrides.Type {
        import factory.convert
        override def toScala: String = s"Map"

        override def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): mapsOps.Map = this

        def toImport: Seq[any.Import] = Seq.empty
      }

      trait CreateMap extends mapsOps.CreateMap with scalaBase.anyOverrides.Expression {
        import factory.convert
        override def toScala: String = {
          val inits = initialKeyValuePairs.map({ case (k, v) => s"${k.toScala} -> ${v.toScala}"}).mkString(", ")
          s"Map[${keyType.toScala},${elementType.toScala}]($inits)"
        }

        override def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): mapsOps.CreateMap =
          copy(keyType = keyType.prefixRootPackage(rootPackageName, excludedTypeNames),
            elementType = elementType.prefixRootPackage(rootPackageName, excludedTypeNames),
            initialKeyValuePairs = initialKeyValuePairs.map({ case (k,v) =>
              (k.prefixRootPackage(rootPackageName, excludedTypeNames), v.prefixRootPackage(rootPackageName, excludedTypeNames))}
            ))
      }

      trait ContainsKeyOp extends mapsOps.ContainsKeyOp with scalaOperatorExpressions.operatorExpressionsOverrides.Operator {
        import factory.convert
        def operator: String = ".containsKey"

        override def toScala(operands: any.Expression*): String = {
          val base = operands(0).toScala
          val key = operands(1).toScala

          s"$base.contains($key)"
        }
      }

      trait GetOp extends mapsOps.GetOp with scalaOperatorExpressions.operatorExpressionsOverrides.Operator {
        import factory.convert

        override def toScala(operands: any.Expression*): String = {
          val base = operands(0).toScala
          val key = operands(1).toScala
          val defaultValue = operands(2).toScala

          s"$base.getOrElse($key, $defaultValue)"
        }
      }

      trait PutOp extends mapsOps.PutOp with scalaOperatorExpressions.operatorExpressionsOverrides.Operator {
        import factory.convert

        override def toScala(operands: any.Expression*): String = {
          val base = operands(0).toScala
          val key = operands(1).toScala
          val value = operands(2).toScala

          s"$base.updated($key, $value)"
        }

        override def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): mapsOps.PutOp =
          copy(keyType = keyType.prefixRootPackage(rootPackageName, excludedTypeNames),
            elementType = elementType.prefixRootPackage(rootPackageName, excludedTypeNames))
      }

      trait Factory extends mapsOps.Factory {}
    }
  }

  override val mapsOpsFactory: scalaMapsOps.mapsOpsOverride.Factory
}

trait FinalMapsAST extends MapsAST { self: FinalOperatorExpressionsAST & FinalBaseAST =>
  object finalMapsFinalTypes {
    trait MapsFinalTypes extends scalaMapsOps.mapsOpsOverride.FinalTypes {
      type Map = scalaMapsOps.mapsOpsOverride.Map
      type CreateMapExpression = scalaMapsOps.mapsOpsOverride.CreateMap
    }
  }

  override val mapsOpsFinalTypes: finalMapsFinalTypes.MapsFinalTypes = new finalMapsFinalTypes.MapsFinalTypes {}

  object finalMapsFactoryTypes {
    trait FinalMapsFactory extends scalaMapsOps.mapsOpsOverride.Factory {
      
      override def map() : mapsOps.Map = {
        case class Map() extends scalaMapsOps.mapsOpsOverride.Map {
          override def getSelfType: scalaBase.anyOverrides.Type = this
          override def getSelfMapType: scalaMapsOps.mapsOpsOverride.Map = this
        }
        Map()
      }

      override def createMap(keyType:any.Type, elementType:any.Type, initialKeyValuePairs: Seq[(any.Expression,any.Expression)]
      ): mapsOps.CreateMap = {
        case class CreateMap(keyType:any.Type, elementType:any.Type, initialKeyValuePairs: Seq[(any.Expression,any.Expression)]) extends
          scalaMapsOps.mapsOpsOverride.CreateMap with finalBaseAST.anyOverrides.FinalExpression{
          override def getSelfCreateMapExpression: scalaMapsOps.mapsOpsOverride.CreateMap = this
        }
        CreateMap(keyType, elementType, initialKeyValuePairs)
      }

      def containsKeyOp(): mapsOps.ContainsKeyOp = {
        case class ContainsKeyOp() extends scalaMapsOps.mapsOpsOverride.ContainsKeyOp with finalOperatorExpressions.operatorExpressionsOverrides.Operator
        ContainsKeyOp()
      }

      def getOp(): mapsOps.GetOp = {
        case class GetOp() extends scalaMapsOps.mapsOpsOverride.GetOp with finalOperatorExpressions.operatorExpressionsOverrides.Operator
        GetOp()
      }

      override def putOp(keyType: any.Type, elementType: any.Type): mapsOps.PutOp = {
        case class PutOp(keyType: any.Type, elementType: any.Type) extends scalaMapsOps.mapsOpsOverride.PutOp with finalOperatorExpressions.operatorExpressionsOverrides.Operator
        PutOp(keyType, elementType)
      }
    }
  }
  
  val mapsOpsFactory: finalMapsFactoryTypes.FinalMapsFactory = new finalMapsFactoryTypes.FinalMapsFactory {}
}
