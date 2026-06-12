package org.combinators.cogen.paradigm.ffi     /*DI:LI:AI*/

import org.combinators.cogen.paradigm.{AnyParadigm, Apply}
import org.combinators.cogen.{Command, Understands}
import Command.Generator

case class CreateMap[Type](keyType:Type, elementType: Type)
case class GetOrElse()
case class ContainsKey()
case class Put[Type](keyType: Type, valueType: Type)

trait Maps[Context] extends FFI {
  import base._
  import syntax._

  trait MapCapabilities {
    implicit val canCreate: Understands[Context, Apply[CreateMap[Type], (Expression,Expression), Expression]]
    def create(keyTpe: Type, elemTpe: Type, values:(Expression,Expression)*): Generator[Context, Expression] =
      AnyParadigm.capability(Apply(CreateMap(keyTpe, elemTpe), values))

    implicit val canContainsKey: Understands[Context, Apply[ContainsKey, Expression, Expression]]
    def contains(map: Expression, key: Expression): Generator[Context, Expression] =
      AnyParadigm.capability(Apply[ContainsKey, Expression, Expression](ContainsKey(), Seq(map, key)))

    implicit val canGet: Understands[Context, Apply[GetOrElse, Expression, Expression]]
    def getOrElse(map: Expression, key: Expression, defaultVal: Expression): Generator[Context, Expression] =
      AnyParadigm.capability(Apply[GetOrElse, Expression, Expression](GetOrElse(), Seq(map, key, defaultVal)))

    implicit val canPut: Understands[Context, Apply[Put[Type], Expression, Expression]]
    def put(map: Expression, keyType: Type, valueType: Type, key:Expression, value:Expression): Generator[Context, Expression] =
      AnyParadigm.capability(Apply[Put[Type], Expression, Expression](Put(keyType, valueType), Seq(map, key, value)))

  }
  val mapCapabilities: MapCapabilities
}

object Maps {
  type WithBase[Ctxt, B <: AnyParadigm] = Maps[Ctxt] { val base: B }
}
