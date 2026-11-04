package org.combinators.ep.language.scala.ast.functional.control

import org.combinators.cogen.Command.Generator
import org.combinators.cogen.paradigm.{Apply, ToTargetLanguageType}
import org.combinators.cogen.{Command, TypeRep, Understands}
import org.combinators.ep.language.inbetween.ContextRegistry
import org.combinators.ep.language.inbetween.any.AnyParadigm
import org.combinators.ep.language.inbetween.ffi.FFI
import org.combinators.ep.language.inbetween.functional.FunctionalParadigm
import org.combinators.ep.language.inbetween.functional.control.Functional as Fun
import org.combinators.ep.language.inbetween.oo.OOParadigm
import org.combinators.ep.language.inbetween.polymorphism.{ParametricPolymorphism, ParametricPolymorphismInADTContexts}
import org.combinators.ep.language.inbetween.polymorphism.generics.Generics
import org.combinators.ep.language.scala.ast.BaseAST

trait Functional extends Fun {
  override val _base: AnyParadigm { val ast: BaseAST }
  val oo : OOParadigm.WithBase[_base.type]
  val parametricPolymorphism: ParametricPolymorphism.WithBase[_base.type]
  val generics : Generics.WithBase[_base.type, oo.type, parametricPolymorphism.type]
  val functional: FunctionalParadigm.WithBase[_base.type]
  val parametricPolymorphismInADTContexts: ParametricPolymorphismInADTContexts.WithBase[_base.type, functional.type]

  val methodRegistry: ContextRegistry[_base.type, _base.ast.any.Method]
  val constructorRegistry: ContextRegistry[_base.type, _base.ast.oo.Constructor]
  val classRegistry: ContextRegistry[_base.type, _base.ast.oo.Class]
  val adtRegistry: ContextRegistry[_base.type, _base.ast.functional.AlgebraicDataType]

  trait ArrowsIn[Ctxt] extends FFI[Ctxt] {
    import _base.ast

    override val base: _base.type = _base
    val canToTargetLanguage: Understands[Ctxt, ToTargetLanguageType[ast.any.Type]]
    val canApplyType: Understands[Ctxt, Apply[ast.any.Type, ast.any.Type, ast.any.Type]]

    override val tpeLookup: TypeRep => Option[Generator[Ctxt, _base.syntax.Type]] = {
      case TypeRep.Arrow(source,target) => Some(
        for {
          srcTpe <- ToTargetLanguageType[ast.any.Type](source).interpret(using canToTargetLanguage)
          targetTpe <- ToTargetLanguageType[ast.any.Type](target).interpret(using canToTargetLanguage)
          arrowTpe <- Command.lift(ast.ooFactory.classReferenceType(_base.ast.scalaBaseFactory.name("Function", "Function")))
          tpe <- Apply[
            ast.any.Type,
            ast.any.Type,
            ast.any.Type](arrowTpe, Seq(srcTpe, targetTpe)).interpret(using canApplyType)
        } yield tpe)
      case _ => None
    }

    override val reifylookup: (tpeRep: TypeRep) => tpeRep.HostType => Option[Generator[Ctxt, _base.syntax.Expression]] = {
      tpe => arr => None
    }
  }

  val arrowsInMethods: ArrowsIn[_base.ast.any.Method] = {
    import _base.ast.any.*
    class ArrowsInMethods(
           override val registry: ContextRegistry[_base.type, Method] = methodRegistry,
           override val canToTargetLanguage: Understands[Method, ToTargetLanguageType[Type]] = _base.methodBodyCapabilities.canTransformTypeInMethodBody,
           override val canApplyType: Understands[Method, Apply[Type, Type, Type]] = parametricPolymorphism.methodBodyCapabilities.canApplyTypeInMethod,
         ) extends ArrowsIn[Method] {
    }
    new ArrowsInMethods()
  }
  
  val arrowsInConstructors: ArrowsIn[_base.ast.oo.Constructor] = {
    import _base.ast.any.*
    import _base.ast.oo.Constructor
    class ArrowsInConstructors(
          override val registry: ContextRegistry[_base.type, Constructor] = constructorRegistry,
          override val canToTargetLanguage: Understands[Constructor, ToTargetLanguageType[Type]] = oo.constructorCapabilities.canTranslateTypeInConstructor,
          override val canApplyType: Understands[Constructor, Apply[Type, Type, Type]] = generics.constructorCapabilities.canApplyTypeInConstructor,
        ) extends ArrowsIn[Constructor] {
    }
    new ArrowsInConstructors()
  }

  val arrowsInClasses: ArrowsIn[_base.ast.oo.Class] = {
    import _base.ast.any.*
    import _base.ast.oo.Class as Cls
    class ArrowsInClasses(
           override val registry: ContextRegistry[_base.type, Cls] = classRegistry,
           override val canToTargetLanguage: Understands[Cls, ToTargetLanguageType[Type]] = oo.classCapabilities.canTranslateTypeInClass,
           override val canApplyType: Understands[Cls, Apply[Type, Type, Type]] = generics.classCapabilities.canApplyTypeInClass,
         ) extends ArrowsIn[Cls] {
    }
    new ArrowsInClasses()
  }
  
  val arrowsInADTs: ArrowsIn[_base.ast.functional.AlgebraicDataType] = {
    import _base.ast.any.*
    import _base.ast.functional.AlgebraicDataType as Adt
    class ArrowsInADTs(
           override val registry: ContextRegistry[_base.type, Adt] = adtRegistry,
           override val canToTargetLanguage: Understands[Adt, ToTargetLanguageType[Type]] = functional.typeCapabilities.canTranslateTypeInType,
           override val canApplyType: Understands[Adt, Apply[Type, Type, Type]] = parametricPolymorphismInADTContexts.algebraicDataTypeCapabilities.canApplyTypeInADT,
         ) extends ArrowsIn[Adt] {
    }
    new ArrowsInADTs()
  }
}

object Functional {
  type WithBase[B <: AnyParadigm, PP <: ParametricPolymorphism.WithBase[B], OO <: OOParadigm.WithBase[B], G <: Generics.WithBase[B, OO, PP],
    F <: FunctionalParadigm.WithBase[B], PA <: ParametricPolymorphismInADTContexts.WithBase[B, F]] =
    Functional {
      val _base: B
      val parametricPolymorphism: PP
      val oo: OO
      val generics: G
      val functional: F
      val parametricPolymorphismInADTContexts: PA
    }
  def apply[AST <: BaseAST, B <: AnyParadigm.WithAST[AST]](
        base: B,
        _oo: OOParadigm.WithBase[base.type],
        _parametricPolymorphism: ParametricPolymorphism.WithBase[base.type],
        _generics: Generics.WithBase[base.type, _oo.type, _parametricPolymorphism.type],
        _functional: FunctionalParadigm.WithBase[base.type],
        _parametricPolymorphismInADTContexts: ParametricPolymorphismInADTContexts.WithBase[base.type, _functional.type],
        _methodRegistry: ContextRegistry[base.type, base.ast.any.Method],
        _constructorRegistry: ContextRegistry[base.type, base.ast.oo.Constructor],
        _classRegistry: ContextRegistry[base.type, base.ast.oo.Class],
        _adtRegistry: ContextRegistry[base.type, base.ast.functional.AlgebraicDataType],
      ): Functional.WithBase[base.type, _parametricPolymorphism.type, _oo.type, _generics.type, _functional.type, _parametricPolymorphismInADTContexts.type] = {
    class Fns(
        override val _base: base.type = base,
        override val parametricPolymorphism: _parametricPolymorphism.type = _parametricPolymorphism,
        override val oo: _oo.type = _oo,
        override val generics: _generics.type = _generics,
        override val functional: _functional.type = _functional,
        override val parametricPolymorphismInADTContexts: _parametricPolymorphismInADTContexts.type = _parametricPolymorphismInADTContexts,
        override val methodRegistry: ContextRegistry[base.type, _base.ast.any.Method] = _methodRegistry,
        override val constructorRegistry: ContextRegistry[base.type, _base.ast.oo.Constructor] = _constructorRegistry,
        override val classRegistry: ContextRegistry[base.type, _base.ast.oo.Class] = _classRegistry,
        override val adtRegistry: _adtRegistry.type = _adtRegistry,
      ) extends Functional {}
    new Fns()
  }
}
