package org.combinators.ep.language.scala.ast.ffi

import org.combinators.cogen.Command.Generator
import org.combinators.cogen.{Command, TypeRep}
import org.combinators.ep.language.inbetween.ContextRegistry
import org.combinators.ep.language.inbetween.any.AnyParadigm
import org.combinators.ep.language.scala.ast.BaseAST
import org.combinators.ep.language.inbetween.ffi.Exceptions as Excpt

import scala.reflect.{ClassTag, classTag}

trait Exceptions[AST <: ExceptionsAST & BaseAST, B <: org.combinators.cogen.paradigm.AnyParadigm, T: ClassTag](
  val _base: AnyParadigm.WithAST[AST] & B,
  matchingTpeRep: TypeRep.OfHostType[T],
  methodRegistry: ContextRegistry[B, _base.ast.any.Method],
  constructorRegistry: ContextRegistry[B, _base.ast.oo.Constructor],
  classRegistry: ContextRegistry[B, _base.ast.oo.Class]
) extends Excpt[AST, B, T] {
  
  val nameProvider: _base.ast.nameProvider.ScalaNameProvider = _base.ast.nameProviderFactory.scalaNameProvider
  
  trait ScalaExceptionsIn[Ctxt](val registry: ContextRegistry[B, Ctxt]) extends super.ExceptionsIn[Ctxt] {
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
  
  val exceptionsInMethods: ScalaExceptionsIn[_base.ast.any.Method] = new ScalaExceptionsIn(methodRegistry) {}
  val exceptionsInConstructors: ScalaExceptionsIn[_base.ast.oo.Constructor] = new ScalaExceptionsIn(constructorRegistry) {}
  val exceptionsInClasses: ScalaExceptionsIn[_base.ast.oo.Class] = new ScalaExceptionsIn(classRegistry) {}
}

object Exceptions {
  type WithBase[T, AST <: ExceptionsAST & BaseAST, B <: AnyParadigm.WithAST[AST]] = Exceptions[AST, B, T] {}
  def apply[T: ClassTag, AST <: ExceptionsAST & BaseAST, B <: AnyParadigm.WithAST[AST]](
    _base: B,
    matchingTpeRep: TypeRep.OfHostType[T],
    methodRegistry: ContextRegistry[B, _base.ast.any.Method],
    constructorRegistry: ContextRegistry[B, _base.ast.oo.Constructor],
    classRegistry: ContextRegistry[B, _base.ast.oo.Class],
  ): WithBase[T, AST, B] = new Exceptions[AST, B, T](_base, matchingTpeRep, methodRegistry, constructorRegistry, classRegistry) with Excpt[AST, B, T](_base) {}
}
