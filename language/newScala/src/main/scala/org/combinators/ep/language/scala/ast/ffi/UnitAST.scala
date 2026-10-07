package org.combinators.ep.language.scala.ast.ffi    /*DI:LD:AI*/

import org.combinators.ep.language.inbetween.ffi.UnitAST as InbetweenUnitAST
import org.combinators.ep.language.scala.ast.{BaseAST, FinalBaseAST}

trait UnitAST extends InbetweenUnitAST { self: BaseAST =>
  object scalaUnitOps {
    object unitOpsOverride {
  
      trait Unit extends unitOps.Unit with scalaBase.anyOverrides.Expression {
        def toScala: String = "()"

        def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): any.Expression = this
      }

      trait Factory extends unitOps.Factory {}
    }
  }
   
  override val unitOpsFactory: scalaUnitOps.unitOpsOverride.Factory
}

trait FinalUnitAST extends UnitAST { self: FinalBaseAST =>
  object finalUnitFactoryTypes {
    trait FinalUnitFactory extends scalaUnitOps.unitOpsOverride.Factory {
    
      def unitExp(): unitOps.Unit = {
        case class Unit() extends scalaUnitOps.unitOpsOverride.Unit
          with finalBaseAST.anyOverrides.FinalExpression {}
        Unit()
      }
    }
  }
  
  val unitOpsFactory: finalUnitFactoryTypes.FinalUnitFactory = new finalUnitFactoryTypes.FinalUnitFactory {}
}
