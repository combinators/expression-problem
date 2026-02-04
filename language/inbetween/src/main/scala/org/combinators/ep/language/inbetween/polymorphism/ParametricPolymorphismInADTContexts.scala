package org.combinators.ep.language.inbetween.polymorphism

import org.combinators.cogen.paradigm.{Apply, ParametricPolymorphismInADTContexts as PPADT}
import org.combinators.cogen.Understands
import org.combinators.ep.language.inbetween.any.AnyParadigm
import org.combinators.ep.language.inbetween.functional.{FunctionalAST, FunctionalParadigm}

trait ParametricPolymorphismInADTContexts[AST <: ParametricPolymorphismAST & FunctionalAST, B, F](val base: AnyParadigm.WithAST[AST] & B, val functional: FunctionalParadigm.WithBase[AST, base.type & B] & F) extends PPADT {
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
  type WithBase[AST <: ParametricPolymorphismAST & FunctionalAST, B <: AnyParadigm.WithAST[AST], F <: FunctionalParadigm.WithBase[AST, B]] = ParametricPolymorphismInADTContexts[AST, B, F] {}

  def apply[AST <: ParametricPolymorphismAST & FunctionalAST, B <: AnyParadigm.WithAST[AST], F <: FunctionalParadigm.WithBase[AST, B]](_base: B, _functional: FunctionalParadigm.WithBase[AST, _base.type] & F): WithBase[AST, B, F] = new ParametricPolymorphismInADTContexts[AST, B, F](_base, _functional) {}
}
