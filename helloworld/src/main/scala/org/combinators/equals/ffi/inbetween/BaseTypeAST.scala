package org.combinators.equals.ffi.inbetween

import org.combinators.ep.language.inbetween.polymorphism.ParametricPolymorphismAST
import org.combinators.equals.CompositeDataType

trait BaseTypeAST extends ParametricPolymorphismAST {
  object baseTypeOps {
    trait FinalTypes {
      type BaseType <: baseTypeOps.BaseType
      type CompositeType <: baseTypeOps.CompositeType
    }
    
    trait BaseType extends any.Type {
      def getSelfBaseType: baseTypeOpsFinalTypes.BaseType
    }

    trait CompositeType extends any.Type {
      def getSelfCompositeType: baseTypeOpsFinalTypes.CompositeType
      
      def model: CompositeDataType
      
      def copyAsCompositeType(model:CompositeDataType = this.model) = {
        baseTypeOpsFactory.compositeType(model)
      }
    }

    trait Factory {
      def baseType(): BaseType
      def compositeType(model:CompositeDataType): CompositeType

      implicit def convert(other: BaseType): baseTypeOpsFinalTypes.BaseType = other.getSelfBaseType
      implicit def convert(other: CompositeType): baseTypeOpsFinalTypes.CompositeType = other.getSelfCompositeType
    }
  }
  
  val baseTypeOpsFinalTypes: baseTypeOps.FinalTypes
  val baseTypeOpsFactory: baseTypeOps.Factory
}