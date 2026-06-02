package org.combinators.ep.language.inbetween.ffi    /*DI:LI:AI*/

import org.combinators.cogen.paradigm.Apply
import org.combinators.cogen.paradigm.ffi.{CreateArray, Get, Length, Set, Arrays as Arys}
import org.combinators.cogen.{Command, Understands}
import org.combinators.cogen.Command.Generator
import org.combinators.ep.language.inbetween.any.AnyParadigm

trait Arrays[AST <: ArraysAST, B](val _base: AnyParadigm.WithAST[AST] & B) {
  trait ArraysInMethods extends Arys[_base.ast.any.Method] {
    val base: _base.type = _base

    import base.ast.arraysOpsFactory
    import base.ast.any

    override val arrayCapabilities: ArrayCapabilities = new ArrayCapabilities {
      override implicit val canCreate: Understands[any.Method, CreateArray[any.Type,any.Expression]] =
        new Understands[any.Method, CreateArray[any.Type, any.Expression]] {
          override def perform(context: any.Method, command: CreateArray[any.Type, any.Expression]): (any.Method, any.Expression) = {
            (context, arraysOpsFactory.createArray(command.elementType, command.dimensions, command.contentSpec))
          }
        }
      override implicit val canGet: Understands[any.Method, Apply[Get, any.Expression, any.Expression]] =
        new Understands[any.Method, Apply[Get, any.Expression, any.Expression]] {
          override def perform(context: any.Method, command: Apply[Get, any.Expression, any.Expression]): (any.Method, any.Expression) = {
            (context, arraysOpsFactory.getArrayOp(command.arguments(0), command.arguments.drop(1)))
          }
        }
      override implicit val canSet: Understands[any.Method, Apply[Set, any.Expression, any.Expression]] =
        new Understands[any.Method, Apply[Set, any.Expression, any.Expression]] {
          override def perform(context: any.Method, command: Apply[Set, any.Expression, any.Expression]): (any.Method, any.Expression) = {
            (context, arraysOpsFactory.setArrayOp(command.arguments(0), command.arguments.drop(2), command.arguments(1)))
          }
        }
      override implicit val canLength: Understands[any.Method, Apply[Length, any.Expression, any.Expression]] =
        new Understands[any.Method, Apply[Length, any.Expression, any.Expression]] {
          override def perform(context: any.Method, command: Apply[Length, any.Expression, any.Expression]): (any.Method, any.Expression) = {
            (context, arraysOpsFactory.lengthArrayExpression(command.arguments(0), command.arguments.drop(1)))
          }
        }
    }
    override def enable(): Generator[any.Project, Unit] = Command.skip[any.Project]
  }
  
  val arraysInMethods: ArraysInMethods = new ArraysInMethods {}
}

object Arrays {
  type WithBase[AST <: ArraysAST, B <: AnyParadigm.WithAST[AST]] = Arrays[AST, B] {}

  def apply[AST <: ArraysAST, B <: AnyParadigm.WithAST[AST]](_base: B): WithBase[AST, B] = new Arrays[AST, B](_base) {}
}

