package org.combinators.ep.language.inbetween.ffi  /*DI:LI:AI*/

import org.combinators.ep.generator.{Command, Understands}
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.Apply
import org.combinators.ep.language.inbetween.any
import org.combinators.ep.language.inbetween.any.AnyParadigm
import org.combinators.ep.generator.paradigm.ffi.{Append, Cons, Create, Head, Tail, Lists => Lsts}
trait Lists[FT <: lists.FinalTypes, FactoryType <: lists.Factory[FT]] extends Lsts[any.Method[FT]] {
  val base: AnyParadigm.WithFT[FT, FactoryType]
  import base.factory

  override val listCapabilities: ListCapabilities = new ListCapabilities {
    override implicit val canCreate: Understands[any.Method[FT], Apply[Create[any.Type[FT]], any.Expression[FT], any.Expression[FT]]] =
      new Understands[any.Method[FT], Apply[Create[any.Type[FT]], any.Expression[FT], any.Expression[FT]]] {
        override def perform(context: any.Method[FT], command: Apply[Create[any.Type[FT]], any.Expression[FT], any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
          (context, factory.createList(command.functional.elementType, command.arguments))
        }
      }
    override implicit val canCons: Understands[any.Method[FT], Apply[Cons, any.Expression[FT], any.Expression[FT]]] =
      new Understands[any.Method[FT], Apply[Cons, any.Expression[FT], any.Expression[FT]]] {
        override def perform(context: any.Method[FT], command: Apply[Cons, any.Expression[FT], any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
          (context, factory.consList(command.arguments(0), command.arguments(1)))
        }
      }
    override implicit val canHead: Understands[any.Method[FT], Apply[Head, any.Expression[FT], any.Expression[FT]]] =
      new Understands[any.Method[FT], Apply[Head, any.Expression[FT], any.Expression[FT]]] {
        override def perform(context: any.Method[FT], command: Apply[Head, any.Expression[FT], any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
          (context, factory.head(command.arguments(0)))
        }
      }
    override implicit val canTail: Understands[any.Method[FT], Apply[Tail, any.Expression[FT], any.Expression[FT]]] =
      new Understands[any.Method[FT], Apply[Tail, any.Expression[FT], any.Expression[FT]]] {
        override def perform(context: any.Method[FT], command: Apply[Tail, any.Expression[FT], any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
          (context, factory.tail(command.arguments(0)))
        }
      }
    override implicit val canAppend: Understands[any.Method[FT], Apply[Append, any.Expression[FT], any.Expression[FT]]] =
      new Understands[any.Method[FT], Apply[Append, any.Expression[FT], any.Expression[FT]]] {
        override def perform(context: any.Method[FT], command: Apply[Append, any.Expression[FT], any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
          (context, factory.appendList(command.arguments(0), command.arguments(1)))
        }
      }
  }

  override def enable(): Generator[any.Project[FT], Unit] = Command.skip[any.Project[FT]]
}

object Lists {
  type WithBase[FT <: lists.FinalTypes, FactoryType <: lists.Factory[FT], B <: AnyParadigm.WithFT[FT, FactoryType]] = Lists[FT, FactoryType] { val base: B }
  def apply[FT <: lists.FinalTypes, FactoryType <: lists.Factory[FT], B <: AnyParadigm.WithFT[FT, FactoryType]](_base: B): WithBase[FT, FactoryType, _base.type] = new Lists[FT, FactoryType] {
    val base: _base.type = _base
  }
}
