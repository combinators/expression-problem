package org.combinators.ep.language.inbetween.ffi    /*DI:LI:AI*/

import org.combinators.cogen.Command.Generator
import org.combinators.cogen.{Command, Understands}
import org.combinators.cogen.paradigm.ffi.Equality as Eqls
import org.combinators.cogen.paradigm.{Apply, ffi}
import org.combinators.ep.language.inbetween.any.AnyParadigm

trait Equals[AST <: EqualsAST, B](val _base: AnyParadigm.WithAST[AST] & B) {
  trait BooleansIn[Ctxt] extends Eqls[Ctxt] {
    override val base: _base.type = _base

    import base.ast.{any, equalsOpFactory}

    val equalityCapabilities: EqualityCapabilities = new EqualityCapabilities {
      implicit val canEquals: Understands[Ctxt, Apply[ffi.Equals[any.Type], any.Expression, any.Expression]] = new Understands[Ctxt, Apply[ffi.Equals[any.Type], any.Expression, any.Expression]] {
        def perform(context: Ctxt, command: Apply[ffi.Equals[any.Type], any.Expression, any.Expression]): (Ctxt, any.Expression) = {
          (context, equalsOpFactory.equals(command.functional.inType, command.arguments.head, command.arguments.tail.head))
        }
      }
    }
    def enable(): Generator[any.Project, Unit] = Command.skip[any.Project]
  }
}

object Equals {
  type WithBase[AST <: EqualsAST, B <: AnyParadigm.WithAST[AST]] = Equals[AST, B] {}

  def apply[AST <: EqualsAST, B <: AnyParadigm.WithAST[AST]](_base: B): WithBase[AST, B] = new Equals[AST, B](_base) {}
}

