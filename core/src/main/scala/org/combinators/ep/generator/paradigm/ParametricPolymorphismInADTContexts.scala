package org.combinators.ep.generator.paradigm

import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.Understands

trait ParametricPolymorphismInADTContexts {
  val base: org.combinators.ep.generator.paradigm.AnyParadigm
  val functional: Functional.WithBase[base.type]

  import base._
  import syntax._
  import functional.TypeContext
  
  trait AlgebraicDataTypeCapabilities {
    implicit val canApplyTypeInADT: Understands[TypeContext, Apply[Type, Type, Type]]
    def applyType(tpe: Type, arguments: Seq[Type]): Generator[TypeContext, Type] =
      AnyParadigm.capability(Apply[Type, Type, Type](tpe, arguments))
  }
  val algebraicDataTypeCapabilities: AlgebraicDataTypeCapabilities
}

object ParametricPolymorphismInADTContexts {
  type WithBase[B <: AnyParadigm, F <: Functional.WithBase[B]] = ParametricPolymorphism { 
    val base: B
    val functional: F
  }
}