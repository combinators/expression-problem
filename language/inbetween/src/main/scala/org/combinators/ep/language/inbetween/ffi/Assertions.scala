package org.combinators.ep.language.inbetween.ffi

/*DI:LI:AI*/

import org.combinators.cogen.paradigm.Apply
import org.combinators.cogen.paradigm.ffi.{Assert, Assertions as Asrts}
import org.combinators.cogen.Command.Generator
import org.combinators.cogen.{Command, Understands}
import org.combinators.ep.language.inbetween.any
import org.combinators.ep.language.inbetween.any.AnyParadigm

// cannot find 'assertions'
trait Assertions[AST <: AssertionsAST, B](val _base: AnyParadigm.WithAST[AST] & B) {
  trait AssertionsInMethods extends Asrts[_base.ast.any.Method] {
    val base: _base.type = _base
    import base.ast.assertionOpsFactory
    import base.ast.any

    val assertionCapabilities: AssertionCapabilities = new AssertionCapabilities {
      implicit val canAssert: Understands[any.Method, Apply[Assert, any.Expression, any.Expression]] =
        new Understands[any.Method, Apply[Assert, any.Expression, any.Expression]] {
          def perform(context: any.Method, command: Apply[Assert, any.Expression, any.Expression]): (any.Method, any.Expression) = {
            (context, assertionOpsFactory.assertTrue(command.arguments.head))
          }
        }
    }
    def enable(): Generator[any.Project, Unit] = Command.skip[any.Project]
  }
  val assertionsInMethods: AssertionsInMethods = new AssertionsInMethods {}
}

object Assertions {
  type WithBase[AST <: AssertionsAST, B <: AnyParadigm.WithAST[AST]] = Assertions[AST, B] {}

  def apply[AST <: AssertionsAST, B <: AnyParadigm.WithAST[AST]](_base: B): WithBase[AST, B] = new Assertions[AST, B](_base) {}
}