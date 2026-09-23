package org.combinators.ep.language.scala.ast.ffi

import org.combinators.cogen.Command.Generator
import org.combinators.cogen.{Command, TypeRep}
import org.combinators.ep.language.inbetween.ContextRegistry
import org.combinators.ep.language.inbetween.any.AnyParadigm
import org.combinators.ep.language.scala.ast.BaseAST
import org.combinators.ep.language.inbetween.ffi.Assertions as Assrt

import scala.reflect.{ClassTag, classTag}

trait Assertions extends Assrt {
  val _base: AnyParadigm { val ast: AssertionsAST & BaseAST }
  val methodRegistry: ContextRegistry[_base.type, _base.ast.any.Method]
  val constructorRegistry: ContextRegistry[_base.type, _base.ast.oo.Constructor]
  val classRegistry: ContextRegistry[_base.type, _base.ast.oo.Class]

  trait ScalaAssertionsIn[Ctxt] extends super.AssertionsIn[Ctxt] {
    override val tpeLookup: TypeRep => Option[Generator[Ctxt, _base.syntax.Type]] =
      _  => None
    override val reifylookup: (tpeRep: TypeRep) => tpeRep.HostType => Option[Generator[Ctxt, _base.syntax.Expression]] =
      _ => _ => None
  }
  
  val assertionsInMethods: ScalaAssertionsIn[_base.ast.any.Method] = {
    class AssertionsInMethods(
      override val registry: ContextRegistry[_base.type, _base.ast.any.Method] = methodRegistry
    ) extends ScalaAssertionsIn[_base.ast.any.Method]
    new AssertionsInMethods() {}
  }
  val assertionsInConstructors: ScalaAssertionsIn[_base.ast.oo.Constructor] = {
    class AssertionsInConstructors(
      override val registry: ContextRegistry[_base.type, _base.ast.oo.Constructor] = constructorRegistry
    ) extends ScalaAssertionsIn[_base.ast.oo.Constructor]
    new AssertionsInConstructors() {}
  }

  val assertionsInClasses: ScalaAssertionsIn[_base.ast.oo.Class] = {
    class AssertionsInClasses(
      override val registry: ContextRegistry[_base.type, _base.ast.oo.Class] = classRegistry
    ) extends ScalaAssertionsIn[_base.ast.oo.Class]
    new AssertionsInClasses() {}
  }
}

object Assertions {
  type WithBase[B <: AnyParadigm] = Assertions { val _base: B }
  def apply[AST <: AssertionsAST & BaseAST, B <: AnyParadigm.WithAST[AST]](
    base: B,
    methodRegistry: ContextRegistry[base.type, base.ast.any.Method],
    constructorRegistry: ContextRegistry[base.type, base.ast.oo.Constructor],
    classRegistry: ContextRegistry[base.type, base.ast.oo.Class],
  ): Assertions.WithBase[base.type] = {
    class Asrts(
      override val _base: base.type,
      override val methodRegistry: ContextRegistry[_base.type, _base.ast.any.Method],
      override val constructorRegistry: ContextRegistry[_base.type, _base.ast.oo.Constructor],
      override val classRegistry: ContextRegistry[_base.type, _base.ast.oo.Class],
    ) extends Assertions {}
    new Asrts(base, methodRegistry, constructorRegistry, classRegistry)
  }
}
