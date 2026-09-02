package org.combinators.ep.language.scala.ast.ffi
import org.combinators.cogen.Command.Generator
import org.combinators.cogen.{Command, TypeRep}
import org.combinators.ep.language.inbetween.ContextRegistry
import org.combinators.ep.language.inbetween.any.AnyParadigm
import org.combinators.ep.language.scala.ast.BaseAST
import org.combinators.ep.language.inbetween.ffi.Arithmetic as Arith

import scala.reflect.{ClassTag, classTag}

trait Arithmetic[AST <: ArithmeticAST & BaseAST, B, T: ClassTag](
  val _base: AnyParadigm.WithAST[AST] & B,
  matchingTpeRep: TypeRep.OfHostType[T],
  methodRegistry: ContextRegistry[B, _base.ast.any.Method],
  constructorRegistry: ContextRegistry[B, _base.ast.oo.Constructor],
  classRegistry: ContextRegistry[B, _base.ast.oo.Class]
) extends Arith[AST, B, T] {
  
  val nameProvider: _base.ast.nameProvider.ScalaNameProvider = _base.ast.nameProviderFactory.scalaNameProvider
  
  trait ScalaArithmeticIn[Ctxt](val registry: ContextRegistry[B, Ctxt]) extends super.ArithmeticIn[Ctxt] {
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
  
  val arithmeticInMethods: ScalaArithmeticIn[_base.ast.any.Method] = new ScalaArithmeticIn(methodRegistry) {}
  val arithmeticInConstructors: ScalaArithmeticIn[_base.ast.oo.Constructor] = new ScalaArithmeticIn(constructorRegistry) {}
  val arithmeticInClasses: ScalaArithmeticIn[_base.ast.oo.Class] = new ScalaArithmeticIn(classRegistry) {}
}

object Arithmetic {
  type WithBase[T, AST <: ArithmeticAST & BaseAST, B <: AnyParadigm.WithAST[AST]] = Arithmetic[AST, B, T] {}
  def apply[T, AST <: ArithmeticAST & BaseAST, B <: AnyParadigm.WithAST[AST]](
    _base: B,
    matchingTpeRep: TypeRep.OfHostType[T],
    methodRegistry: ContextRegistry[B, _base.ast.any.Method],
    constructorRegistry: ContextRegistry[B, _base.ast.oo.Constructor],
    classRegistry: ContextRegistry[B, _base.ast.oo.Class],
  ): WithBase[T, AST, B] = new Arithmetic[AST, B, T](_base, matchingTpeRep, methodRegistry, constructorRegistry, classRegistry) with Arith[AST, B, T](_base) {}
}
