package org.combinators.ep.language.inbetween.ffi

/*DI:LI:AI*/

// cannot find 'lists'

import org.combinators.cogen.paradigm.Apply
import org.combinators.cogen.paradigm.ffi.{Append, Cons, Create, Head, Tail, Lists as Lsts}
import org.combinators.cogen.{Command, Understands}
import org.combinators.cogen.Command.Generator
import org.combinators.ep.language.inbetween.{any, polymorphism}
import org.combinators.ep.language.inbetween.any.{AnyParadigm, AnyParadigm2}
trait Lists2(val _base: AnyParadigm2.WithAST[ListsAST]) {
  trait ListsInMethods extends Lsts[_base.ast.any.Method] {
    val base: _base.type = _base

    import base.ast.listsOpsFactory
    import base.ast.any

    override val listCapabilities: ListCapabilities = new ListCapabilities {
      override implicit val canCreate: Understands[any.Method, Apply[Create[any.Type], any.Expression, any.Expression]] =
        new Understands[any.Method, Apply[Create[any.Type], any.Expression, any.Expression]] {
          override def perform(context: any.Method, command: Apply[Create[any.Type], any.Expression, any.Expression]): (any.Method, any.Expression) = {
            (context, listsOpsFactory.createList(command.functional.elementType, command.arguments))
          }
        }
      override implicit val canCons: Understands[any.Method, Apply[Cons, any.Expression, any.Expression]] =
        new Understands[any.Method, Apply[Cons, any.Expression, any.Expression]] {
          override def perform(context: any.Method, command: Apply[Cons, any.Expression, any.Expression]): (any.Method, any.Expression) = {
            (context, listsOpsFactory.consList(command.arguments(0), command.arguments(1)))
          }
        }
      override implicit val canHead: Understands[any.Method, Apply[Head, any.Expression, any.Expression]] =
        new Understands[any.Method, Apply[Head, any.Expression, any.Expression]] {
          override def perform(context: any.Method, command: Apply[Head, any.Expression, any.Expression]): (any.Method, any.Expression) = {
            (context, listsOpsFactory.head(command.arguments(0)))
          }
        }
      override implicit val canTail: Understands[any.Method, Apply[Tail, any.Expression, any.Expression]] =
        new Understands[any.Method, Apply[Tail, any.Expression, any.Expression]] {
          override def perform(context: any.Method, command: Apply[Tail, any.Expression, any.Expression]): (any.Method, any.Expression) = {
            (context, listsOpsFactory.tail(command.arguments(0)))
          }
        }
      override implicit val canAppend: Understands[any.Method, Apply[Append, any.Expression, any.Expression]] =
        new Understands[any.Method, Apply[Append, any.Expression, any.Expression]] {
          override def perform(context: any.Method, command: Apply[Append, any.Expression, any.Expression]): (any.Method, any.Expression) = {
            (context, listsOpsFactory.appendList(command.arguments(0), command.arguments(1)))
          }
        }
    }
    override def enable(): Generator[any.Project, Unit] = Command.skip[any.Project]
  }
  
  val listsInMethods: ListsInMethods = new ListsInMethods {}
}

object Lists2 {
  type WithBase[AST <: ListsAST, B <: AnyParadigm2.WithAST[AST]] = Lists2 {val _base: B}

  trait WB[AST <: ListsAST, B <: AnyParadigm2.WithAST[AST]](override val _base: B) extends Lists2 {}

  def apply[AST <: ListsAST, B <: AnyParadigm2.WithAST[AST]](_base: B): WithBase[AST, _base.type] = new WB[AST, _base.type](_base) with Lists2(_base) {}
}

