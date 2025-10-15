package org.combinators.ep.language.inbetween.ffi

/*DI:LI:AI*/

import org.combinators.cogen.paradigm.Apply
import org.combinators.cogen.paradigm.ffi.{GetStringLength, StringAppend, ToString, Strings as Strs}
import org.combinators.cogen.{Command, Understands}
import org.combinators.cogen.Command.Generator
import org.combinators.ep.language.inbetween.any
import org.combinators.ep.language.inbetween.any.{AnyParadigm, AnyParadigm2}

// cannot find 'strings'
trait Strings2(val _base: AnyParadigm2.WithAST[StringsAST]) {
  trait StringsInMethods extends Strs[_base.ast.any.Method] {
    override val base: _base.type = _base

    import base.ast.any
    import base.ast.stringOpsFactory

    val stringCapabilities: StringCapabilities = new StringCapabilities {
      implicit val canGetStringLength: Understands[any.Method, Apply[GetStringLength, any.Expression, any.Expression]] =
        new Understands[any.Method, Apply[GetStringLength, any.Expression, any.Expression]] {
          def perform(context: any.Method, command: Apply[GetStringLength, any.Expression, any.Expression]): (any.Method, any.Expression) = {
            (context, stringOpsFactory.stringLength(command.arguments.head))
          }
        }

      implicit val canAppend: Understands[any.Method, Apply[StringAppend, any.Expression, any.Expression]] =
        new Understands[any.Method, Apply[StringAppend, any.Expression, any.Expression]] {
          def perform(context: any.Method, command: Apply[StringAppend, any.Expression, any.Expression]): (any.Method, any.Expression) = {
            (context, command.arguments.tail.foldLeft(command.arguments.head) { case (r, l) => stringOpsFactory.appendString(r, l) })
          }
        }
      implicit val canToStringInCtxt: Understands[any.Method, Apply[ToString[any.Type], any.Expression, any.Expression]] =
        new Understands[any.Method, Apply[ToString[any.Type], any.Expression, any.Expression]] {
          def perform(context: any.Method, command: Apply[ToString[any.Type], any.Expression, any.Expression]): (any.Method, any.Expression) = {
            (context, stringOpsFactory.toString(command.arguments.head))
          }
        }
    }
    def enable(): Generator[any.Project, Unit] = Command.skip[any.Project]
  }

  val stringsInMethods: StringsInMethods = new StringsInMethods {}
}

object Strings2 {
  type WithBase[AST <: StringsAST, B <: AnyParadigm2.WithAST[AST]] = Strings2 {val _base: B}

  trait WB[AST <: StringsAST, B <: AnyParadigm2.WithAST[AST]](override val _base: B) extends Strings2 {}

  def apply[AST <: StringsAST, B <: AnyParadigm2.WithAST[AST]](_base: B): WithBase[AST, _base.type] = new WB[AST, _base.type](_base) with Strings2(_base) {}
}

