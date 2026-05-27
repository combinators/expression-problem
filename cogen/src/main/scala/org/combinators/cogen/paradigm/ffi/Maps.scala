package org.combinators.cogen.paradigm.ffi

import org.combinators.cogen.paradigm.{AnyParadigm, Apply}
import org.combinators.cogen.{Command, Understands}
import Command.Generator

case class CreateMap[Type](keyType:Type, elementType: Type)
case class GetOrElse()
case class Put()

trait Maps[Context] extends FFI {
  import base._
  import syntax._

  trait MapCapabilities {
    implicit val canCreate: Understands[Context, Apply[CreateMap[Type], (Expression,Expression), Expression]]
    def create(keyTpe: Type, elemTpe: Type, values:(Expression,Expression)*): Generator[Context, Expression] =
      AnyParadigm.capability(Apply(CreateMap(keyTpe, elemTpe), values))

    implicit val canGet: Understands[Context, Apply[GetOrElse, Expression, Expression]]
    def getOrElse(map: Expression, key: Expression, defaultVal: Expression): Generator[Context, Expression] =
      AnyParadigm.capability(Apply[GetOrElse, Expression, Expression](GetOrElse(), Seq(map, key, defaultVal)))

    implicit val canPut: Understands[Context, Apply[Put, Expression, Expression]]
    def put(map: Expression, key:Expression, value:Expression): Generator[Context, Expression] =
      AnyParadigm.capability(Apply[Put, Expression, Expression](Put(), Seq(map, key, value)))

  }
  val mapCapabilities: MapCapabilities
}

object Maps {
  type WithBase[Ctxt, B <: AnyParadigm] = Maps[Ctxt] { val base: B }
}
