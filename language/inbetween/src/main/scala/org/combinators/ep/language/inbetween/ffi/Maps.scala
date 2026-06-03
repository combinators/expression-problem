package org.combinators.ep.language.inbetween.ffi    /*DI:LI:AI*/

import org.combinators.cogen.paradigm.Apply
import org.combinators.cogen.paradigm.ffi.{ContainsKey, CreateMap, GetOrElse, Put, Maps as Mps}
import org.combinators.cogen.{Command, Understands}
import org.combinators.cogen.Command.Generator
import org.combinators.ep.language.inbetween.{any, polymorphism}
import org.combinators.ep.language.inbetween.any.AnyParadigm

trait Maps[AST <: MapsAST, B](val _base: AnyParadigm.WithAST[AST] & B) {
  // TODO: These are defined in Method context. What about constructor? What about Class context when needing to add Field?
  trait MapsIn[Ctxt] extends Mps[Ctxt] {
    val base: _base.type = _base

    import base.ast.mapsOpsFactory
    import base.ast.any

    override val mapCapabilities: MapCapabilities = new MapCapabilities {
      override implicit val canCreate: Understands[Ctxt, Apply[CreateMap[any.Type], (any.Expression,any.Expression), any.Expression]] =
        new Understands[Ctxt, Apply[CreateMap[any.Type], (any.Expression,any.Expression), any.Expression]] {
          override def perform(context: Ctxt, command: Apply[CreateMap[any.Type], (any.Expression, any.Expression), any.Expression]): (Ctxt, any.Expression) = {
            (context, mapsOpsFactory.createMap(command.functional.keyType, command.functional.elementType, command.arguments))
          }
        }
      override implicit val canContainsKey: Understands[Ctxt, Apply[ContainsKey, any.Expression, any.Expression]] =
          new Understands[Ctxt, Apply[ContainsKey, any.Expression, any.Expression]] {
            override def perform(context: Ctxt, command: Apply[ContainsKey, any.Expression, any.Expression]): (Ctxt, any.Expression) = {
              (context, mapsOpsFactory.containsKey(command.arguments(0), command.arguments(1)))
            }
          }
      override implicit val canGet: Understands[Ctxt, Apply[GetOrElse, any.Expression, any.Expression]] =
        new Understands[Ctxt, Apply[GetOrElse, any.Expression, any.Expression]] {
          override def perform(context: Ctxt, command: Apply[GetOrElse, any.Expression, any.Expression]): (Ctxt, any.Expression) = {
            (context, mapsOpsFactory.get(command.arguments(0), command.arguments(1), command.arguments(2)))
          }
        }
      override implicit val canPut: Understands[Ctxt, Apply[Put, any.Expression, any.Expression]] =
        new Understands[Ctxt, Apply[Put, any.Expression, any.Expression]] {
          override def perform(context: Ctxt, command: Apply[Put, any.Expression, any.Expression]): (Ctxt, any.Expression) = {
            (context, mapsOpsFactory.put(command.arguments(0), command.arguments(1), command.arguments(2)))
          }
        }
    }
    override def enable(): Generator[any.Project, Unit] = Command.skip[any.Project]
  }

  // TODO: These are only for methods. What about constructors? Which are based on OO concept? And so might not be part of inBetween?
  
  def mapsIn[Ctxt]: MapsIn[Ctxt] = new MapsIn[Ctxt] {}
  val mapsInMethods:MapsIn[_base.ast.any.Method] = mapsIn[_base.ast.any.Method]
}

object Maps {
  type WithBase[AST <: MapsAST, B <: AnyParadigm.WithAST[AST]] = Maps[AST, B] {}

  def apply[AST <: MapsAST, B <: AnyParadigm.WithAST[AST]](_base: B): WithBase[AST, B] = new Maps[AST, B](_base) {}
}

