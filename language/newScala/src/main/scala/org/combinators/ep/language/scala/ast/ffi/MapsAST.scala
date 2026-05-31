package org.combinators.ep.language.scala.ast.ffi

import org.combinators.ep.language.inbetween.ffi.MapsAST as InbetweenMapsAST
import org.combinators.ep.language.scala.ast.{BaseAST, FinalBaseAST}
import org.combinators.cogen.{FileWithPath, TypeRep}

trait MapsAST extends InbetweenMapsAST { self: OperatorExpressionsAST & BaseAST =>
  object scalaMapsOps {
    object mapsOpsOverride {

      trait CreateMap extends mapsOps.CreateMap with scalaBase.anyOverrides.Type {
        override def toScala: String = "Seq"

        override def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): CreateMap =
          this
        def toImport: Seq[any.Import] = Seq.empty
      }

      trait GetOp extends mapsOps.GetOp with scalaOperatorExpressions.operatorExpressionsOverrides.Operator with scalaOperatorExpressions.InfixOperator {
        def operator: String = ".get"
      }

      trait PutOp extends mapsOps.PutOp with scalaOperatorExpressions.operatorExpressionsOverrides.Operator with scalaOperatorExpressions.PostfixOperator {
        def operator: String = ".put"
      }

      trait Factory extends mapsOps.Factory {}
    }

    def mapReificationExtensions(tpe: TypeRep)(value: tpe.HostType): Option[String] = {
      tpe match {
        case t: TypeRep.Map.type =>
          value match {
            case m:Map[tpe.HostType, tpe.HostType] => Some(s"org.combinators.ep.util.Node(id, Seq(2,3)})")
            case _ => ???
          }
        case _ => None
      }
    }

  }

  override val mapsOpsFactory: scalaMapsOps.mapsOpsOverride.Factory
}

trait FinalMapsAST extends MapsAST { self: FinalOperatorExpressionsAST & FinalBaseAST =>
  object finalMapsFactoryTypes {
    trait FinalMapsFactory extends scalaMapsOps.mapsOpsOverride.Factory {
      def createMap(): mapsOps.CreateMap = {
        case class CreateMap() extends scalaMapsOps.mapsOpsOverride.CreateMap {
          def getSelfType: scalaBase.anyOverrides.Type = this
        }
        CreateMap()
      }
      def getOp(): mapsOps.GetOp = {
        case class GetOp() extends scalaMapsOps.mapsOpsOverride.GetOp with finalOperatorExpressions.operatorExpressionsOverrides.Operator
        GetOp()
      }

      def putOp(): mapsOps.PutOp = {
        case class PutOp() extends scalaMapsOps.mapsOpsOverride.PutOp with finalOperatorExpressions.operatorExpressionsOverrides.Operator
        PutOp()
      }
    }
  }
  
  val mapsOpsFactory: finalMapsFactoryTypes.FinalMapsFactory = new finalMapsFactoryTypes.FinalMapsFactory {}
}
