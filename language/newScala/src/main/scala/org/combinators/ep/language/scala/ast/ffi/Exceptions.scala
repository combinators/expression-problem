package org.combinators.ep.language.scala.ast.ffi

import org.combinators.cogen.Command.Generator
import org.combinators.cogen.{Command, TypeRep}
import org.combinators.ep.language.inbetween.ContextRegistry
import org.combinators.ep.language.inbetween.any.AnyParadigm
import org.combinators.ep.language.scala.ast.BaseAST
import org.combinators.ep.language.inbetween.ffi.Exceptions as Excpt

import scala.reflect.{ClassTag, classTag}

trait Exceptions[AST <: ExceptionsAST & BaseAST, B <: org.combinators.cogen.paradigm.AnyParadigm] extends Excpt[AST, B] {
  val methodRegistry: ContextRegistry[_base.type, _base.ast.any.Method]
  val constructorRegistry: ContextRegistry[_base.type, _base.ast.oo.Constructor]
  val classRegistry: ContextRegistry[_base.type, _base.ast.oo.Class]

  trait ScalaExceptionsIn[Ctxt] extends super.ExceptionsIn[Ctxt] {
    override val tpeLookup: TypeRep => Option[Generator[Ctxt, _base.syntax.Type]] =
      _ => None
    override val reifylookup: (tpeRep: TypeRep) => tpeRep.HostType => Option[Generator[Ctxt, _base.syntax.Expression]] =
      _ => _ => None
  }

  val exceptionsInMethods: ScalaExceptionsIn[_base.ast.any.Method] = {
    class ExIn(
      override val registry: ContextRegistry[_base.type, _base.ast.any.Method] = methodRegistry
    ) extends ScalaExceptionsIn[_base.ast.any.Method]
    new ExIn() {}
  }
  val exceptionsInConstructors: ScalaExceptionsIn[_base.ast.oo.Constructor] = {
    class ExIn(
      override val registry: ContextRegistry[_base.type, _base.ast.oo.Constructor] = constructorRegistry
    ) extends ScalaExceptionsIn[_base.ast.oo.Constructor]
    new ExIn() {}
  }

  val exceptionsInClasses: ScalaExceptionsIn[_base.ast.oo.Class] = {
    class ExIn(
      override val registry: ContextRegistry[_base.type, _base.ast.oo.Class] = classRegistry
    ) extends ScalaExceptionsIn[_base.ast.oo.Class]
    new ExIn() {}
  }
}

object Exceptions {
  def apply[AST <: ExceptionsAST & BaseAST, B <: AnyParadigm.WithAST[AST]](
    base: B,
    methodRegistry: ContextRegistry[base.type, base.ast.any.Method],
    constructorRegistry: ContextRegistry[base.type, base.ast.oo.Constructor],
    classRegistry: ContextRegistry[base.type, base.ast.oo.Class],
  ): Exceptions[base.ast.type, base.type] = {
    class Ex(
      override val _base: base.type,
      override val methodRegistry: ContextRegistry[_base.type, _base.ast.any.Method],
      override val constructorRegistry: ContextRegistry[_base.type, _base.ast.oo.Constructor],
      override val classRegistry: ContextRegistry[_base.type, _base.ast.oo.Class],
    ) extends Exceptions[base.ast.type, base.type] {}
    new Ex(base, methodRegistry, constructorRegistry, classRegistry)
  }
}
