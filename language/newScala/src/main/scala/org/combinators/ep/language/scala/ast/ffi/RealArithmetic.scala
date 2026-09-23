package org.combinators.ep.language.scala.ast.ffi

import org.combinators.cogen.Command.Generator
import org.combinators.cogen.{Command, TypeRep}
import org.combinators.ep.language.inbetween.ContextRegistry
import org.combinators.ep.language.inbetween.any.AnyParadigm
import org.combinators.ep.language.scala.ast.BaseAST
import org.combinators.ep.language.inbetween.ffi.RealArithmetic as RealArith

import scala.reflect.{ClassTag, classTag}

trait RealArithmetic[AST <: RealArithmeticAST & BaseAST, B <: org.combinators.cogen.paradigm.AnyParadigm, T: ClassTag] extends RealArith[AST, B, T] {
  val matchingTpeRep: TypeRep.OfHostType[T]
  val methodRegistry: ContextRegistry[_base.type, _base.ast.any.Method]
  val constructorRegistry: ContextRegistry[_base.type, _base.ast.oo.Constructor]
  val classRegistry: ContextRegistry[_base.type, _base.ast.oo.Class]
 
  val nameProvider: _base.ast.nameProvider.ScalaNameProvider = _base.ast.nameProviderFactory.scalaNameProvider

  trait ScalaRealArithmeticIn[Ctxt] extends super.RealArithmeticIn[Ctxt] {
    override val registry: ContextRegistry[_base.type, Ctxt]
    override val tpeLookup: TypeRep => Option[Generator[Ctxt, _base.syntax.Type]] =
      tpeRep => if (tpeRep == matchingTpeRep) {
        Some(Command.lift(_base.ast.ooFactory.classReferenceType(nameProvider.mangle(classTag[T].runtimeClass.getName))))
      } else None
    override val reifylookup: (tpeRep: TypeRep) => tpeRep.HostType => Option[Generator[Ctxt, _base.syntax.Expression]] = {
      tpeRep => if (tpeRep == matchingTpeRep) {
        (value: tpeRep.HostType) => Some(Command.lift(_base.ast.scalaBaseFactory.reifiedScalaValue(tpeRep, value, None)))
      } else value => None
    }
  }

  val realArithmeticInMethods: ScalaRealArithmeticIn[_base.ast.any.Method] = {
    class Arith(
      override val registry: methodRegistry.type = methodRegistry
    ) extends ScalaRealArithmeticIn[_base.ast.any.Method] {}
    new Arith()
  }
  val realArithmeticInConstructors: ScalaRealArithmeticIn[_base.ast.oo.Constructor] = {
    class Arith(
      override val registry: methodRegistry.type = methodRegistry
    ) extends ScalaRealArithmeticIn[_base.ast.oo.Constructor] {}
    new Arith()
  }
  val realArithmeticInClasses: ScalaRealArithmeticIn[_base.ast.oo.Class] = {
    class Arith(
      override val registry: methodRegistry.type = methodRegistry
    ) extends ScalaRealArithmeticIn[_base.ast.oo.Class] {}
    new Arith()
  }
}

object RealArithmetic {
  def apply[AST <: RealArithmeticAST & BaseAST, B <: AnyParadigm.WithAST[AST], T: ClassTag](
    base: B,
    matchingTpeRep: TypeRep.OfHostType[T],
    methodRegistry: ContextRegistry[base.type, base.ast.any.Method],
    constructorRegistry: ContextRegistry[base.type, base.ast.oo.Constructor],
    classRegistry: ContextRegistry[base.type, base.ast.oo.Class],
  ): RealArithmetic[base.ast.type, base.type, T] = {
    class Arith(
      override val _base: base.type,
      override val matchingTpeRep: TypeRep.OfHostType[T],
      override val methodRegistry: ContextRegistry[_base.type, _base.ast.any.Method],
      override val constructorRegistry: ContextRegistry[_base.type, _base.ast.oo.Constructor],
      override val classRegistry: ContextRegistry[_base.type, _base.ast.oo.Class],
    ) extends RealArithmetic[base.ast.type, base.type, T]
    new Arith(base, matchingTpeRep, methodRegistry, constructorRegistry, classRegistry) {}
  }
}
