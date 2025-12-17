package org.combinators.ep.language.inbetween.ffi

/*DI:LI:AI*/

import org.combinators.cogen.Command.Generator
import org.combinators.cogen.{Command, Understands}
import org.combinators.cogen.paradigm.ffi.{Equality as Eqls, *}
import org.combinators.cogen.paradigm.{Apply, ffi}
import org.combinators.ep.language.inbetween.any
import org.combinators.ep.language.inbetween.any.AnyParadigm2


trait Equals2[AST <: EqualsAST, B](val _base: AnyParadigm2.WithAST[AST] & B) {
  trait BooleansInMethods extends Eqls[_base.ast.any.Method] {
    override val base: _base.type = _base

    import base.ast.{any, equalsOpFactory}

    val equalityCapabilities: EqualityCapabilities = new EqualityCapabilities {
      implicit val canEquals: Understands[any.Method, Apply[ffi.Equals[any.Type], any.Expression, any.Expression]] = new Understands[any.Method, Apply[ffi.Equals[any.Type], any.Expression, any.Expression]] {
        def perform(context: any.Method, command: Apply[ffi.Equals[any.Type], any.Expression, any.Expression]): (any.Method, any.Expression) = {
          (context, equalsOpFactory.equals(command.functional.inType, command.arguments.head, command.arguments.tail.head))
        }
      }
    }
    def enable(): Generator[any.Project, Unit] = Command.skip[any.Project]
  }
  val equalsInMethods: BooleansInMethods = new BooleansInMethods {} 
}

object Equals2 {
  type WithBase[AST <: EqualsAST, B <: AnyParadigm2.WithAST[AST]] = Equals2[AST, B] {}

  def apply[AST <: EqualsAST, B <: AnyParadigm2.WithAST[AST]](_base: B): WithBase[AST, B] = new Equals2[AST, B](_base) {}
}

