package org.combinators.ep.language.inbetween.polymorphism

import org.combinators.cogen.paradigm.{Apply, ParametricPolymorphismInADTContexts as PPADT}
import org.combinators.cogen.{Command, Understands}
import org.combinators.ep.language.inbetween.any.{AnyParadigm, AnyParadigm2}
import org.combinators.ep.language.inbetween.{any, functional as fun}
import org.combinators.ep.language.inbetween.functional.{FunctionalAST, FunctionalParadigm, FunctionalParadigm2}

trait ParametricPolymorphismInADTContexts2(val base: AnyParadigm2.WithAST[ParametricPolymorphismAST & FunctionalAST], val functional: FunctionalParadigm2.WithBase[base.ast.type, base.type]) extends PPADT {
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
  type WithBase[AST <: ParametricPolymorphismAST & FunctionalAST, B <: AnyParadigm2.WithAST[AST], F[A<: ParametricPolymorphismAST & FunctionalAST, FB <: AnyParadigm2.WithAST[A]] <: FunctionalParadigm2.WithBase[A, FB]] = ParametricPolymorphismInADTContexts2 { val base: B; val functional: F[base.ast.type, base.type] }

  trait WB[AST <: ParametricPolymorphismAST & FunctionalAST, B <: AnyParadigm2.WithAST[AST], F[A<: ParametricPolymorphismAST & FunctionalAST, FB <: AnyParadigm2.WithAST[A]] <: FunctionalParadigm2.WithBase[A, FB]](override val base: B, override val functional: F[base.ast.type, base.type]) extends ParametricPolymorphismInADTContexts2 {}

  def apply[AST <: ParametricPolymorphismAST & FunctionalAST, B <: AnyParadigm2.WithAST[AST], F[A<: ParametricPolymorphismAST & FunctionalAST, FB <: AnyParadigm2.WithAST[A]]  <: FunctionalParadigm2.WithBase[A, FB]](_base: B, _functional: F[_base.ast.type, _base.type]): WithBase[AST, B, F] = new WB[AST, _base.type, F](_base, _functional) with ParametricPolymorphismInADTContexts2(_base, _functional) {}
}
