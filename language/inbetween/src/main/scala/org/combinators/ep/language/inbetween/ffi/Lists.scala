package org.combinators.ep.language.inbetween.ffi    /*DI:LI:AI*/

import org.combinators.cogen.paradigm.Apply
import org.combinators.cogen.paradigm.ffi.{Append, Cons, Create, Head, Tail, Lists as Lsts}
import org.combinators.cogen.{Command, Understands}
import org.combinators.cogen.Command.Generator
import org.combinators.ep.language.inbetween.any.AnyParadigm

trait Lists[AST <: ListsAST, B](val _base: AnyParadigm.WithAST[AST] & B) {
  trait ListsIn[Ctxt] extends Lsts[Ctxt] {
    val base: _base.type = _base

    import base.ast.listsOpsFactory
    import base.ast.any

    override val listCapabilities: ListCapabilities = new ListCapabilities {
      override implicit val canCreate: Understands[Ctxt, Apply[Create[any.Type], any.Expression, any.Expression]] =
        new Understands[Ctxt, Apply[Create[any.Type], any.Expression, any.Expression]] {
          override def perform(context: Ctxt, command: Apply[Create[any.Type], any.Expression, any.Expression]): (Ctxt, any.Expression) = {
            (context, listsOpsFactory.createList(command.functional.elementType, command.arguments))
          }
        }
      override implicit val canCons: Understands[Ctxt, Apply[Cons, any.Expression, any.Expression]] =
        new Understands[Ctxt, Apply[Cons, any.Expression, any.Expression]] {
          override def perform(context: Ctxt, command: Apply[Cons, any.Expression, any.Expression]): (Ctxt, any.Expression) = {
            (context, listsOpsFactory.consList(command.arguments(0), command.arguments(1)))
          }
        }
      override implicit val canHead: Understands[Ctxt, Apply[Head, any.Expression, any.Expression]] =
        new Understands[Ctxt, Apply[Head, any.Expression, any.Expression]] {
          override def perform(context: Ctxt, command: Apply[Head, any.Expression, any.Expression]): (Ctxt, any.Expression) = {
            (context, listsOpsFactory.head(command.arguments(0)))
          }
        }
      override implicit val canTail: Understands[Ctxt, Apply[Tail, any.Expression, any.Expression]] =
        new Understands[Ctxt, Apply[Tail, any.Expression, any.Expression]] {
          override def perform(context: Ctxt, command: Apply[Tail, any.Expression, any.Expression]): (Ctxt, any.Expression) = {
            (context, listsOpsFactory.tail(command.arguments(0)))
          }
        }
      override implicit val canAppend: Understands[Ctxt, Apply[Append, any.Expression, any.Expression]] =
        new Understands[Ctxt, Apply[Append, any.Expression, any.Expression]] {
          override def perform(context: Ctxt, command: Apply[Append, any.Expression, any.Expression]): (Ctxt, any.Expression) = {
            (context, listsOpsFactory.appendList(command.arguments(0), command.arguments(1)))
          }
        }
    }
  }
  
}

object Lists {
  type WithBase[AST <: ListsAST, B <: AnyParadigm.WithAST[AST]] = Lists[AST, B] {}

  def apply[AST <: ListsAST, B <: AnyParadigm.WithAST[AST]](_base: B): WithBase[AST, B] = new Lists[AST, B](_base) {}
}

