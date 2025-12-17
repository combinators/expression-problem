package org.combinators.ep.language.inbetween.polymorphism

import org.combinators.cogen.paradigm.{Apply, ParametricPolymorphismInADTContexts as PPADT}
import org.combinators.cogen.{Command, Understands}
import org.combinators.ep.language.inbetween.any.{AnyParadigm, AnyParadigm2}
import org.combinators.ep.language.inbetween.{any, functional as fun}
import org.combinators.ep.language.inbetween.functional.{FunctionalAST, FunctionalParadigm, FunctionalParadigm2}

trait ParametricPolymorphismInADTContexts2[AST <: ParametricPolymorphismAST & FunctionalAST, B, F](val base: AnyParadigm2.WithAST[AST] & B, val functional: FunctionalParadigm2.WithBase[AST, base.type & B] & F) extends PPADT {
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

object ParametricPolymorphismInADTContexts2 {
  type WithBase[AST <: ParametricPolymorphismAST & FunctionalAST, B <: AnyParadigm2.WithAST[AST], F <: FunctionalParadigm2.WithBase[AST, B]] = ParametricPolymorphismInADTContexts2[AST, B, F] {}

  def apply[AST <: ParametricPolymorphismAST & FunctionalAST, B <: AnyParadigm2.WithAST[AST], F <: FunctionalParadigm2.WithBase[AST, B]](_base: B, _functional: FunctionalParadigm2.WithBase[AST, _base.type] & F): WithBase[AST, B, F] = new ParametricPolymorphismInADTContexts2[AST, B, F](_base, _functional) {}
}
