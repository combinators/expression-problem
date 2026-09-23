package org.combinators.ep.language.scala.ast.ffi

import org.combinators.cogen.Command.Generator
import org.combinators.cogen.{Command, TypeRep}
import org.combinators.ep.language.inbetween.ContextRegistry
import org.combinators.ep.language.inbetween.any.AnyParadigm
import org.combinators.ep.language.inbetween.any.AnyParadigm.WithAST
import org.combinators.ep.language.scala.ast.BaseAST
import org.combinators.ep.language.inbetween.ffi.Arithmetic as Arith

import scala.reflect.{ClassTag, classTag}

trait Arithmetic[AST <: ArithmeticAST & BaseAST, B <: org.combinators.cogen.paradigm.AnyParadigm, T: ClassTag] extends Arith[AST, B, T] {
  override val _base: AnyParadigm.WithAST[AST] & B
  val matchingTpeRep: TypeRep.OfHostType[T]
  val methodRegistry: ContextRegistry[_base.type, _base.ast.any.Method]
  val constructorRegistry: ContextRegistry[_base.type, _base.ast.oo.Constructor]
  val classRegistry: ContextRegistry[_base.type, _base.ast.oo.Class]
  
  val nameProvider: _base.ast.nameProvider.ScalaNameProvider = _base.ast.nameProviderFactory.scalaNameProvider
  
  trait ScalaArithmeticIn[Ctxt] extends super.ArithmeticIn[Ctxt] {
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
  
  val arithmeticInMethods: ScalaArithmeticIn[_base.ast.any.Method] = new ScalaArithmeticIn { 
    override val registry: methodRegistry.type = methodRegistry 
  }
  val arithmeticInConstructors: ScalaArithmeticIn[_base.ast.oo.Constructor] = new ScalaArithmeticIn {
    override val registry: constructorRegistry.type = constructorRegistry
  }
  val arithmeticInClasses: ScalaArithmeticIn[_base.ast.oo.Class] = new ScalaArithmeticIn {
    override val registry: classRegistry.type = classRegistry
  }
}

object Arithmetic {
  type WithBase[T, AST <: ArithmeticAST & BaseAST, B <: AnyParadigm.WithAST[AST]] = Arithmetic[AST, B, T] {}
  def apply[T: ClassTag, AST <: ArithmeticAST & BaseAST, B <: AnyParadigm.WithAST[AST]](
    base: B,
    _matchingTpeRep: TypeRep.OfHostType[T],
    _methodRegistry: ContextRegistry[B, base.ast.any.Method],
    _constructorRegistry: ContextRegistry[B, base.ast.oo.Constructor],
    _classRegistry: ContextRegistry[B, base.ast.oo.Class],
  ): WithBase[T, AST, B] = new Arithmetic[AST, B, T] with Arith[AST, B, T] {
    override val _base: base.type = base
    val matchingTpeRep: _matchingTpeRep.type = _matchingTpeRep
    val methodRegistry: _methodRegistry.type = _methodRegistry
    val constructorRegistry: _constructorRegistry.type = _constructorRegistry
    val classRegistry: _classRegistry.type = _classRegistry    
  }
}
