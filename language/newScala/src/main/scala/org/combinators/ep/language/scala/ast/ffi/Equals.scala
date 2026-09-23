package org.combinators.ep.language.scala.ast.ffi

import org.combinators.cogen.Command.Generator
import org.combinators.cogen.{Command, TypeRep}
import org.combinators.ep.language.inbetween.{ContextRegistry, ffi}
import org.combinators.ep.language.inbetween.any.AnyParadigm
import org.combinators.ep.language.scala.ast.BaseAST
import org.combinators.ep.language.inbetween.ffi.Equals as Eql

import scala.reflect.{ClassTag, classTag}

trait Equals extends Eql {
  override val _base: AnyParadigm { val ast: EqualsAST & BaseAST }
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
  type WithBase[B <: AnyParadigm] = Equals { val _base: B } 
  def apply[AST <: EqualsAST & BaseAST, B <: AnyParadigm.WithAST[AST]](
    base: B,
    _methodRegistry: ContextRegistry[base.type, base.ast.any.Method],
    _constructorRegistry: ContextRegistry[base.type, base.ast.oo.Constructor],
    _classRegistry: ContextRegistry[base.type, base.ast.oo.Class],
  ): Equals.WithBase[base.type] = {
    class Eqls(
      override val _base: base.type = base,
      override val methodRegistry: ContextRegistry[_base.type, _base.ast.any.Method] = _methodRegistry,
      override val constructorRegistry: ContextRegistry[_base.type, _base.ast.oo.Constructor] = _constructorRegistry,
      override val classRegistry: ContextRegistry[_base.type, _base.ast.oo.Class] = _classRegistry,
    ) extends Equals {}
    new Eqls()
  }
}
