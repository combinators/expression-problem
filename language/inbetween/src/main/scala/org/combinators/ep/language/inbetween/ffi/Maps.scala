package org.combinators.ep.language.inbetween.ffi

/*DI:LI:AI*/

import org.combinators.cogen.paradigm.Apply
import org.combinators.cogen.paradigm.ffi.{CreateMap, GetOrElse, Put, Maps as Mps}
import org.combinators.cogen.{Command, Understands}
import org.combinators.cogen.Command.Generator
import org.combinators.ep.language.inbetween.{any, polymorphism}
import org.combinators.ep.language.inbetween.any.AnyParadigm

trait Maps[AST <: MapsAST, B](val _base: AnyParadigm.WithAST[AST] & B) {
  // TODO: These are defined in Method context. What about constructor? What about Class context when needing to add Field?
  trait MapsInMethods extends Mps[_base.ast.any.Method] {
    val base: _base.type = _base

    import base.ast.mapsOpsFactory
    import base.ast.any

    override val mapCapabilities: MapCapabilities = new MapCapabilities {
      override implicit val canCreate: Understands[any.Method, Apply[CreateMap[any.Type], (any.Expression,any.Expression), any.Expression]] =
        new Understands[any.Method, Apply[CreateMap[any.Type], (any.Expression,any.Expression), any.Expression]] {
          override def perform(context: any.Method, command: Apply[CreateMap[any.Type], (any.Expression, any.Expression), any.Expression]): (any.Method, any.Expression) = {
            (context, mapsOpsFactory.createMap(command.functional.keyType, command.arguments.flatMap(pair => Seq(pair._1, pair._2))))
          }
        }
      override implicit val canGet: Understands[any.Method, Apply[GetOrElse, any.Expression, any.Expression]] =
        new Understands[any.Method, Apply[GetOrElse, any.Expression, any.Expression]] {
          override def perform(context: any.Method, command: Apply[GetOrElse, any.Expression, any.Expression]): (any.Method, any.Expression) = {
            (context, mapsOpsFactory.get(command.arguments(0), command.arguments(1)))
          }
        }
      override implicit val canPut: Understands[any.Method, Apply[Put, any.Expression, any.Expression]] =
        new Understands[any.Method, Apply[Put, any.Expression, any.Expression]] {
          override def perform(context: any.Method, command: Apply[Put, any.Expression, any.Expression]): (any.Method, any.Expression) = {
            (context, mapsOpsFactory.put(command.arguments(0), command.arguments(1), command.arguments(2)))
          }
        }
    }
    override def enable(): Generator[any.Project, Unit] = Command.skip[any.Project]
  }

  // TODO: These are only for methods. What about constructors? Which are based on OO concept? And so might not be part of inBetween?
  
  val mapsInMethods: MapsInMethods = new MapsInMethods {}
}

object Maps {
  type WithBase[AST <: MapsAST, B <: AnyParadigm.WithAST[AST]] = Maps[AST, B] {}

  def apply[AST <: MapsAST, B <: AnyParadigm.WithAST[AST]](_base: B): WithBase[AST, B] = new Maps[AST, B](_base) {}
}

