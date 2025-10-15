package org.combinators.ep.language.inbetween.ffi

/*DI:LI:AI*/

import org.combinators.cogen.paradigm.Apply
import org.combinators.cogen.paradigm.ffi.{Assert, Assertions as Asrts}
import org.combinators.cogen.Command.Generator
import org.combinators.cogen.{Command, Understands}
import org.combinators.ep.language.inbetween.any
import org.combinators.ep.language.inbetween.any.{AnyParadigm, AnyParadigm2}

// cannot find 'assertions'
trait Assertions2(val _base: AnyParadigm2.WithAST[AssertionsAST]) {
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

object Assertions2 {
  type WithBase[AST <: AssertionsAST, B <: AnyParadigm2.WithAST[AST]] = Assertions2 {val _base: B}

  trait WB[AST <: AssertionsAST, B <: AnyParadigm2.WithAST[AST]](override val _base: B) extends Assertions2 {}

  def apply[AST <: AssertionsAST, B <: AnyParadigm2.WithAST[AST]](_base: B): WithBase[AST, _base.type] = new WB[AST, _base.type](_base) with Assertions2(_base) {}
}