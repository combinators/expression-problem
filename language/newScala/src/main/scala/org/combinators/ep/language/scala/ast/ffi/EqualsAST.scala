package org.combinators.ep.language.scala.ast.ffi

import org.combinators.ep.language.inbetween.ffi.{EqualsAST => InbetweenEqualsAST}
import org.combinators.ep.language.scala.ast.{BaseAST, FinalBaseAST}

trait EqualsAST extends InbetweenEqualsAST { self: BaseAST =>
  object scalaEqualsOp {
    object equalsOpOverride {
      trait Equals extends equalsOp.Equals with scalaBase.anyOverrides.Expression {
        import factory.*
        def toScala: String = s"${left.toScala} == ${right.toScala}"

        override def prefixRootPackage(rootPackageName: Seq[any.Name], excludedTypeNames: Set[Seq[any.Name]]): equalsOp.Equals =
          copy(
            tpe = tpe.prefixRootPackage(rootPackageName, excludedTypeNames),
            left = left.prefixRootPackage(rootPackageName, excludedTypeNames),
            right = right.prefixRootPackage(rootPackageName, excludedTypeNames)
          )
      }
      
      trait Factory extends equalsOp.Factory {}
    }
  }

  override val equalsOpFactory: scalaEqualsOp.equalsOpOverride.Factory
}

trait FinalEqualsAST extends EqualsAST {
  self: FinalBaseAST =>
  object finalEqualsFactoryTypes {
    trait EqualsOpFactory extends scalaEqualsOp.equalsOpOverride.Factory {
      def equals(tpe: any.Type, left: any.Expression, right: any.Expression): equalsOp.Equals = {
        case class Equals(tpe: any.Type, left: any.Expression, right: any.Expression) extends scalaEqualsOp.equalsOpOverride.Equals
          with finalBaseAST.anyOverrides.FinalExpression {}
        Equals(tpe, left, right)
      }
    }
  }

  override val equalsOpFactory: scalaEqualsOp.equalsOpOverride.Factory = new finalEqualsFactoryTypes.EqualsOpFactory {}
}