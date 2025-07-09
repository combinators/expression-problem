package org.combinators.ep.language.inbetween.polymorphism

import org.combinators.cogen.paradigm.{Apply, ParametricPolymorphismInADTContexts as PPADT}
import org.combinators.cogen.{Command, Understands}
import org.combinators.ep.language.inbetween.any.AnyParadigm
import org.combinators.ep.language.inbetween.{any, functional => fun}
import org.combinators.ep.language.inbetween.functional.FunctionalParadigm



trait ParametricPolymorphismInADTContexts[FT <: FinalTypes & fun.FinalTypes, FactoryType <: Factory[FT] & fun.Factory[FT]] extends PPADT {
  val base: AnyParadigm.WithFT[FT, FactoryType]
  import base.{FT=>_, FactoryType=>_, _}
  import syntax._

  val functional: FunctionalParadigm.WithBase[FT, FactoryType, base.type]
  lazy val factory: base.factory.type = base.factory

  override val algebraicDataTypeCapabilities: AlgebraicDataTypeCapabilities = new AlgebraicDataTypeCapabilities {
    implicit val canApplyTypeInADT: Understands[fun.AlgebraicDataType[FT], Apply[any.Type[FT], any.Type[FT], any.Type[FT]]] = new Understands[fun.AlgebraicDataType[FT], Apply[any.Type[FT], any.Type[FT], any.Type[FT]]] {
      def perform(context: fun.AlgebraicDataType[FT], command: Apply[any.Type[FT], any.Type[FT], any.Type[FT]]): (fun.AlgebraicDataType[FT], any.Type[FT]) = {
        (context, factory.typeApplication(command.functional, command.arguments))
      }
    }
  }
}

object ParametricPolymorphismInADTContexts {
  type WithBase[FT <: FinalTypes & fun.FinalTypes, FactoryType <: Factory[FT] & fun.Factory[FT], B <: AnyParadigm.WithFT[FT, FactoryType], F <: FunctionalParadigm.WithBase[FT, FactoryType, B]] =
    ParametricPolymorphismInADTContexts[FT, FactoryType] { val base: B; val functional: F }
  def apply[FT <: FinalTypes & fun.FinalTypes, FactoryType <: Factory[FT] & fun.Factory[FT], B <: AnyParadigm.WithFT[FT, FactoryType]](_base: B)(_functional: FunctionalParadigm.WithBase[FT, FactoryType, _base.type]): WithBase[FT, FactoryType, _base.type, _functional.type] = new ParametricPolymorphismInADTContexts[FT, FactoryType] {
    val base: _base.type = _base
    val functional: _functional.type = _functional
  }
}
