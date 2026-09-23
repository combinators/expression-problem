package org.combinators.ep.language.scala.ast.ffi

import org.combinators.cogen.Command.Generator
import org.combinators.cogen.{Command, TypeRep}
import org.combinators.ep.language.inbetween.ContextRegistry
import org.combinators.ep.language.inbetween.any.AnyParadigm
import org.combinators.ep.language.scala.ast.BaseAST
import org.combinators.ep.language.inbetween.ffi.Equals as Eql

import scala.reflect.{ClassTag, classTag}

trait Equals[AST <: EqualsAST & BaseAST, B <: org.combinators.cogen.paradigm.AnyParadigm] extends Eql[AST, B] {
  val methodRegistry: ContextRegistry[_base.type, _base.ast.any.Method]
  val constructorRegistry: ContextRegistry[_base.type, _base.ast.oo.Constructor]
  val classRegistry: ContextRegistry[_base.type, _base.ast.oo.Class]

  trait ScalaEqualsIn[Ctxt] extends super.EqualsIn[Ctxt] {
    override val tpeLookup: TypeRep => Option[Generator[Ctxt, _base.syntax.Type]] =
      _ => None
    override val reifylookup: (tpeRep: TypeRep) => tpeRep.HostType => Option[Generator[Ctxt, _base.syntax.Expression]] =
      _ => _ => None
  }

  val equalsInMethods: ScalaEqualsIn[_base.ast.any.Method] = {
    class EqIn(
      override val registry: ContextRegistry[_base.type, _base.ast.any.Method] = methodRegistry
    ) extends ScalaEqualsIn[_base.ast.any.Method]
    new EqIn() {}
  }
  val equalsInConstructors: ScalaEqualsIn[_base.ast.oo.Constructor] = {
    class EqIn(
      override val registry: ContextRegistry[_base.type, _base.ast.oo.Constructor] = constructorRegistry
    ) extends ScalaEqualsIn[_base.ast.oo.Constructor]
    new EqIn() {}
  }

  val equalsInClasses: ScalaEqualsIn[_base.ast.oo.Class] = {
    class EqIn(
      override val registry: ContextRegistry[_base.type, _base.ast.oo.Class] = classRegistry
    ) extends ScalaEqualsIn[_base.ast.oo.Class]
    new EqIn() {}
  }
}

object Equals {
  def apply[AST <: EqualsAST & BaseAST, B <: AnyParadigm.WithAST[AST]](
    base: B,
    methodRegistry: ContextRegistry[base.type, base.ast.any.Method],
    constructorRegistry: ContextRegistry[base.type, base.ast.oo.Constructor],
    classRegistry: ContextRegistry[base.type, base.ast.oo.Class],
  ): Equals[base.ast.type, base.type] = {
    class Eqls(
      override val _base: base.type,
      override val methodRegistry: ContextRegistry[_base.type, _base.ast.any.Method],
      override val constructorRegistry: ContextRegistry[_base.type, _base.ast.oo.Constructor],
      override val classRegistry: ContextRegistry[_base.type, _base.ast.oo.Class],
    ) extends Equals[base.ast.type, base.type] {}
    new Eqls(base, methodRegistry, constructorRegistry, classRegistry)
  }
}
