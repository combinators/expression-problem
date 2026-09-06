package org.combinators.ep.language.scala.ast.ffi

import org.combinators.cogen.Command.Generator
import org.combinators.cogen.{Command, TypeRep}
import org.combinators.ep.language.inbetween.ContextRegistry
import org.combinators.ep.language.inbetween.any.AnyParadigm
import org.combinators.ep.language.scala.ast.BaseAST
import org.combinators.ep.language.inbetween.ffi.Booleans as Bools

import scala.reflect.{ClassTag, classTag}

trait Booleans[AST <: BooleanAST & BaseAST, B <: org.combinators.cogen.paradigm.AnyParadigm, T: ClassTag](
  val _base: AnyParadigm.WithAST[AST] & B,
  matchingTpeRep: TypeRep.OfHostType[T],
  methodRegistry: ContextRegistry[B, _base.ast.any.Method],
  constructorRegistry: ContextRegistry[B, _base.ast.oo.Constructor],
  classRegistry: ContextRegistry[B, _base.ast.oo.Class]
) extends Bools[AST, B, T] {
  
  val nameProvider: _base.ast.nameProvider.ScalaNameProvider = _base.ast.nameProviderFactory.scalaNameProvider
  
  trait ScalaBooleansIn[Ctxt](val registry: ContextRegistry[B, Ctxt]) extends super.BooleansIn[Ctxt] {
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
  
  val booleansInMethods: ScalaBooleansIn[_base.ast.any.Method] = new ScalaBooleansIn(methodRegistry) {}
  val booleansInConstructors: ScalaBooleansIn[_base.ast.oo.Constructor] = new ScalaBooleansIn(constructorRegistry) {}
  val booleansInClasses: ScalaBooleansIn[_base.ast.oo.Class] = new ScalaBooleansIn(classRegistry) {}
}

object Booleans {
  type WithBase[T, AST <: BooleanAST & BaseAST, B <: AnyParadigm.WithAST[AST]] = Booleans[AST, B, T] {}
  def apply[T: ClassTag, AST <: BooleanAST & BaseAST, B <: AnyParadigm.WithAST[AST]](
    _base: B,
    matchingTpeRep: TypeRep.OfHostType[T],
    methodRegistry: ContextRegistry[B, _base.ast.any.Method],
    constructorRegistry: ContextRegistry[B, _base.ast.oo.Constructor],
    classRegistry: ContextRegistry[B, _base.ast.oo.Class],
  ): WithBase[T, AST, B] = new Booleans[AST, B, T](_base, matchingTpeRep, methodRegistry, constructorRegistry, classRegistry) with Bools[AST, B, T](_base) {}
}
