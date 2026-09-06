package org.combinators.ep.language.inbetween.ffi    /*DI:LI:AI*/

import org.combinators.cogen.paradigm.Apply
import org.combinators.cogen.paradigm.ffi.{Assert, Assertions as Asrts}
import org.combinators.cogen.Understands
import org.combinators.ep.language.inbetween.any.AnyParadigm

trait Assertions[AST <: AssertionsAST, B, T](val _base: AnyParadigm.WithAST[AST] & B) {
  trait AssertionsIn[Ctxt] extends Asrts[Ctxt, T] with FFI[Ctxt] {
    import base.ast.any
    import base.ast.assertionOpsFactory
    override val base: _base.type = _base

    val assertionCapabilities: AssertionCapabilities = new AssertionCapabilities {
      implicit val canAssert: Understands[Ctxt, Apply[Assert, any.Expression, any.Expression]] =
        new Understands[Ctxt, Apply[Assert, any.Expression, any.Expression]] {
          def perform(context: Ctxt, command: Apply[Assert, any.Expression, any.Expression]): (Ctxt, any.Expression) = {
            (context, assertionOpsFactory.assertTrue(command.arguments.head))
          }
        }
    }
  }
}

object Assertions {
  type WithBase[T, AST <: AssertionsAST, B <: AnyParadigm.WithAST[AST]] = Assertions[AST, B, T] {}

  def apply[T, AST <: AssertionsAST, B <: AnyParadigm.WithAST[AST]](_base: B): WithBase[T, AST, B] = new Assertions[AST, B, T](_base) {}
}