package org.combinators.ep.language.inbetween.polymorphism    /*DI:LI:AI*/

import org.combinators.cogen.paradigm.{Apply, ParametricPolymorphismInADTContexts as PPADT}
import org.combinators.cogen.Understands
import org.combinators.ep.language.inbetween.any.AnyParadigm
import org.combinators.ep.language.inbetween.functional.{FunctionalAST, FunctionalParadigm}

trait ParametricPolymorphismInADTContexts extends PPADT {
  val base: AnyParadigm { val ast: ParametricPolymorphismAST & FunctionalAST }
  val functional: FunctionalParadigm.WithBase[base.type]
  import base.ast.any
  import base.ast.polymorphismFactory

  override val algebraicDataTypeCapabilities: AlgebraicDataTypeCapabilities = new AlgebraicDataTypeCapabilities {
    implicit val canApplyTypeInADT: Understands[functional.TypeContext, Apply[any.Type, any.Type, any.Type]] = new Understands[functional.TypeContext, Apply[any.Type, any.Type, any.Type]] {
      def perform(context: functional.TypeContext, command: Apply[any.Type, any.Type, any.Type]): (functional.TypeContext, any.Type) = {
        (context, polymorphismFactory.typeApplication(command.functional, command.arguments))
      }
    }
  }
}

object ParametricPolymorphismInADTContexts {
  type WithBase[B <: AnyParadigm, F <: FunctionalParadigm.WithBase[B]] = ParametricPolymorphismInADTContexts {
    val base: B
    val functional: F
  }

  def apply[AST <: ParametricPolymorphismAST & FunctionalAST, B <: AnyParadigm.WithAST[AST]](
    _base: B,
    _functional: FunctionalParadigm.WithBase[_base.type]
  ): WithBase[_base.type, _functional.type] = {
    class PP(
      override val base: _base.type = _base,
      override val functional: _functional.type = _functional
    ) extends ParametricPolymorphismInADTContexts {}
    new PP()
  } 
}
