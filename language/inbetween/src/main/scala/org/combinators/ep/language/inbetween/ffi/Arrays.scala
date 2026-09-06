package org.combinators.ep.language.inbetween.ffi    /*DI:LI:AI*/

import org.combinators.cogen.paradigm.Apply
import org.combinators.cogen.paradigm.ffi.{CreateArray, Get, Length, Set, Arrays as Arys}
import org.combinators.cogen.Understands
import org.combinators.ep.language.inbetween.any.AnyParadigm

trait Arrays[AST <: ArraysAST, B, T](val _base: AnyParadigm.WithAST[AST] & B) {
  trait ArraysIn[Ctxt] extends Arys[Ctxt, T] with FFI[Ctxt] {
    import base.ast.any
    import base.ast.arraysOpsFactory
    override val base: _base.type = _base

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
  type WithBase[T, AST <: ArraysAST, B <: AnyParadigm.WithAST[AST]] = Arrays[AST, B, T] {}

  def apply[T, AST <: ArraysAST, B <: AnyParadigm.WithAST[AST]](_base: B): WithBase[T, AST, B] = new Arrays[AST, B, T](_base) {}
}

