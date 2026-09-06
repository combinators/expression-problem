package org.combinators.ep.language.inbetween.ffi    /*DI:LI:AI*/

import org.combinators.cogen.Understands
import org.combinators.cogen.paradigm.ffi.Equality as Eqls
import org.combinators.cogen.paradigm.{Apply, ffi}
import org.combinators.ep.language.inbetween.any.AnyParadigm

trait Equals[AST <: EqualsAST, B, T](val _base: AnyParadigm.WithAST[AST] & B) {
  trait EqualsIn[Ctxt] extends Eqls[Ctxt, T] with FFI[Ctxt] {
    import base.ast.any
    import base.ast.equalsOpFactory
    override val base: _base.type = _base

    val equalityCapabilities: EqualityCapabilities = new EqualityCapabilities {
      implicit val canEquals: Understands[Ctxt, Apply[ffi.Equals[any.Type], any.Expression, any.Expression]] = new Understands[Ctxt, Apply[ffi.Equals[any.Type], any.Expression, any.Expression]] {
        def perform(context: Ctxt, command: Apply[ffi.Equals[any.Type], any.Expression, any.Expression]): (Ctxt, any.Expression) = {
          (context, equalsOpFactory.equals(command.functional.inType, command.arguments.head, command.arguments.tail.head))
        }
      }
    }
  }
}

object Equals {
  type WithBase[T, AST <: EqualsAST, B <: AnyParadigm.WithAST[AST]] = Equals[AST, B, T] {}

  def apply[T, AST <: EqualsAST, B <: AnyParadigm.WithAST[AST]](_base: B): WithBase[T, AST, B] = new Equals[AST, B, T](_base) {}
}

