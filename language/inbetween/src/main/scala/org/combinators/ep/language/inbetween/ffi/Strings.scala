package org.combinators.ep.language.inbetween.ffi    /*DI:LI:AI*/

import org.combinators.cogen.paradigm.Apply
import org.combinators.cogen.paradigm.ffi.{GetCharAt, GetStringLength, StringAppend, SubString, ToString, Strings as Strs}
import org.combinators.cogen.{Command, Understands}
import org.combinators.cogen.Command.Generator
import org.combinators.ep.language.inbetween.any.AnyParadigm

trait Strings[AST <: StringAST, B](val _base: AnyParadigm.WithAST[AST] & B) {
  trait StringsIn[Ctxt] extends Strs[Ctxt] {
    override val base: _base.type = _base

    import base.ast.any
    import base.ast.stringOpsFactory

    val stringCapabilities: StringCapabilities = new StringCapabilities {
      implicit val canAppend: Understands[Ctxt, Apply[StringAppend, any.Expression, any.Expression]] =
        new Understands[Ctxt, Apply[StringAppend, any.Expression, any.Expression]] {
          def perform(context: Ctxt, command: Apply[StringAppend, any.Expression, any.Expression]): (Ctxt, any.Expression) = {
            (context, command.arguments.tail.foldLeft(command.arguments.head) { case (r, l) => stringOpsFactory.appendString(r, l) })
          }
        }

      implicit val canGetCharAt: Understands[Ctxt, Apply[GetCharAt, any.Expression, any.Expression]] =
        new Understands[Ctxt, Apply[GetCharAt, any.Expression, any.Expression]] {
          def perform(context: Ctxt, command: Apply[GetCharAt, any.Expression, any.Expression]): (Ctxt, any.Expression) = {
            (context, stringOpsFactory.getCharAt(command.arguments.head, command.arguments.tail.head))
          }
        }
      
      implicit val canGetStringLength: Understands[Ctxt, Apply[GetStringLength, any.Expression, any.Expression]] =
        new Understands[Ctxt, Apply[GetStringLength, any.Expression, any.Expression]] {
          def perform(context: Ctxt, command: Apply[GetStringLength, any.Expression, any.Expression]): (Ctxt, any.Expression) = {
            (context, stringOpsFactory.stringLength(command.arguments.head))
          }
        }

      implicit val canSubString: Understands[Ctxt, Apply[SubString, any.Expression, any.Expression]] =
          new Understands[Ctxt, Apply[SubString, any.Expression, any.Expression]] {
            def perform(context: Ctxt, command: Apply[SubString, any.Expression, any.Expression]): (Ctxt, any.Expression) = {
              (context, stringOpsFactory.subString(command.arguments.head, command.arguments.tail.head, command.arguments.tail.tail.head))
            }
          }
          
      implicit val canToStringInCtxt: Understands[Ctxt, Apply[ToString[any.Type], any.Expression, any.Expression]] =
        new Understands[Ctxt, Apply[ToString[any.Type], any.Expression, any.Expression]] {
          def perform(context: Ctxt, command: Apply[ToString[any.Type], any.Expression, any.Expression]): (Ctxt, any.Expression) = {
            (context, stringOpsFactory.toString(command.arguments.head))
          }
        }
    }
  }
}

object Strings {
  type WithBase[AST <: StringAST, B <: AnyParadigm.WithAST[AST]] = Strings[AST, B] {}
  def apply[AST <: StringAST, B <: AnyParadigm.WithAST[AST]](_base: B): WithBase[AST, B] = new Strings[AST, B](_base) {}
}

