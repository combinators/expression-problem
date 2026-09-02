package org.combinators.ep.language.inbetween.ffi    /*DI:LI:AI*/

import org.combinators.cogen.paradigm.Apply
import org.combinators.cogen.paradigm.ffi.{CreateArray, Get, Length, Set, Arrays as Arys}
import org.combinators.cogen.{Command, TypeRep, Understands}
import org.combinators.cogen.Command.Generator
import org.combinators.ep.language.inbetween.any
import org.combinators.ep.language.inbetween.any.AnyParadigm

trait Arrays[AST <: ArraysAST, B](val _base: AnyParadigm.WithAST[AST] & B) {
  trait ArraysInCtxt[Ctxt] extends Arys[Ctxt] {
    val base: _base.type = _base
  
    import base.ast.arraysOpsFactory
    import base.ast.any

    override val arrayCapabilities: ArrayCapabilities = new ArrayCapabilities {
      override implicit val canCreate: Understands[Ctxt, CreateArray[any.Type,any.Expression]] =
        new Understands[Ctxt, CreateArray[any.Type, any.Expression]] {
          override def perform(context: Ctxt, command: CreateArray[any.Type, any.Expression]): (Ctxt, any.Expression) = {
            (context, arraysOpsFactory.createArray(command.elementType, command.dimensions, command.contentSpec))
          }
        }
      override implicit val canGet: Understands[Ctxt, Apply[Get, any.Expression, any.Expression]] =
        new Understands[Ctxt, Apply[Get, any.Expression, any.Expression]] {
          override def perform(context: Ctxt, command: Apply[Get, any.Expression, any.Expression]): (Ctxt, any.Expression) = {
            (context, arraysOpsFactory.getArrayOp(command.arguments(0), command.arguments.drop(1)))
          }
        }
      override implicit val canSet: Understands[Ctxt, Apply[Set, any.Expression, any.Expression]] =
        new Understands[Ctxt, Apply[Set, any.Expression, any.Expression]] {
          override def perform(context: Ctxt, command: Apply[Set, any.Expression, any.Expression]): (Ctxt, any.Expression) = {
            (context, arraysOpsFactory.setArrayOp(command.arguments(0), command.arguments.drop(2), command.arguments(1)))
          }
        }
      override implicit val canLength: Understands[Ctxt, Apply[Length, any.Expression, any.Expression]] =
        new Understands[Ctxt, Apply[Length, any.Expression, any.Expression]] {
          override def perform(context: Ctxt, command: Apply[Length, any.Expression, any.Expression]): (Ctxt, any.Expression) = {
            (context, arraysOpsFactory.lengthArrayExpression(command.arguments(0), command.arguments.drop(1)))
          }
        }
    }
  }  
}

object Arrays {
  type WithBase[AST <: ArraysAST, B <: AnyParadigm.WithAST[AST]] = Arrays[AST, B] {}

  def apply[AST <: ArraysAST, B <: AnyParadigm.WithAST[AST]](_base: B): WithBase[AST, B] = new Arrays[AST, B](_base) {}
}

