package org.combinators.ep.language.scala.ast.ffi

import org.combinators.cogen.Command.Generator
import org.combinators.cogen.paradigm.AnyParadigm.syntax.forEach
import org.combinators.cogen.paradigm.{Apply, Reify, ToTargetLanguageType}
import org.combinators.cogen.{Command, TypeRep, Understands}
import org.combinators.ep.language.inbetween.{ContextRegistry, ffi}
import org.combinators.ep.language.inbetween.any.AnyParadigm
import org.combinators.ep.language.scala.ast.BaseAST
import org.combinators.ep.language.inbetween.ffi.Maps as Mps
import org.combinators.ep.language.inbetween.oo.OOParadigm
import org.combinators.ep.language.inbetween.polymorphism.ParametricPolymorphism
import org.combinators.ep.language.inbetween.polymorphism.generics.Generics

import scala.reflect.{ClassTag, classTag}

trait Maps extends Mps {
  override val _base: AnyParadigm { val ast: MapsAST & BaseAST }
  val parametricPolymorphism: ParametricPolymorphism.WithBase[_base.type]
  val oo: OOParadigm.WithBase[_base.type]
  val generics: Generics.WithBase[_base.type, oo.type, parametricPolymorphism.type]

  val nameProvider: _base.ast.nameProvider.ScalaNameProvider = _base.ast.nameProviderFactory.scalaNameProvider

  val methodRegistry: ContextRegistry[_base.type, _base.ast.any.Method]
  val constructorRegistry: ContextRegistry[_base.type, _base.ast.oo.Constructor]
  val classRegistry: ContextRegistry[_base.type, _base.ast.oo.Class]

  trait ScalaMapsIn[Ctxt] extends super.MapsIn[Ctxt] {
    import _base.ast
    val canToTargetLanguage: Understands[Ctxt, ToTargetLanguageType[ast.any.Type]]
    val canApplyType: Understands[Ctxt, Apply[ast.any.Type, ast.any.Type, ast.any.Type]]
    def canReifyInCtxt[T]: Understands[Ctxt, Reify[T, ast.any.Expression]]

    override val tpeLookup: TypeRep => Option[Generator[Ctxt, _base.syntax.Type]] = {
      case TypeRep.Map(keyTpe, valueTpe) =>
        val result = for {
          kt <- ToTargetLanguageType(keyTpe).interpret(using canToTargetLanguage)
          vt <- ToTargetLanguageType(valueTpe).interpret(using canToTargetLanguage)
          mapTpe <- Command.lift(ast.ooFactory.classReferenceType(nameProvider.mangle("Map")))
          tpe <- Apply[
            ast.any.Type,
            ast.any.Type,
            ast.any.Type](mapTpe, Seq(kt, vt)).interpret(using canApplyType)
        } yield tpe

        Some(result)
      case _ => None
    }
    override val reifylookup: (tpeRep: TypeRep) => tpeRep.HostType => Option[Generator[Ctxt, _base.syntax.Expression]] = {
      case TypeRep.Map(keyTpe, valueTpe) => elems => {
        val result: Generator[Ctxt, _base.syntax.Expression] = for {
          reifiedElems <- forEach(elems.asInstanceOf[Map[keyTpe.HostType, valueTpe.HostType]].toSeq) { case (k, v) =>
            for {
              key <- Reify(keyTpe, k).interpret(using canReifyInCtxt)
              value <- Reify(valueTpe, v).interpret(using canReifyInCtxt)
            } yield (key, value)
          }
          kt <- ToTargetLanguageType(keyTpe).interpret(using canToTargetLanguage)
          vt <- ToTargetLanguageType(valueTpe).interpret(using canToTargetLanguage)
        } yield _base.ast.mapsOpsFactory.createMap(kt, vt, reifiedElems)
        Some(result)
      }
      case _ => _ => None
    }
  }

  val mapsInMethods: ScalaMapsIn[_base.ast.any.Method] = {
    import _base.ast.any._
    class Mps(
      override val registry: ContextRegistry[_base.type, Method] = methodRegistry,
      override val canToTargetLanguage: Understands[Method, ToTargetLanguageType[Type]] = _base.methodBodyCapabilities.canTransformTypeInMethodBody,
      override val canApplyType: Understands[Method, Apply[Type, Type, Type]] = parametricPolymorphism.methodBodyCapabilities.canApplyTypeInMethod,
    ) extends ScalaMapsIn[Method] {
      override def canReifyInCtxt[T]: Understands[Method, Reify[T, Expression]] = _base.methodBodyCapabilities.canReifyInMethodBody
    }
    new Mps()
  }
  val mapsInConstructors: ScalaMapsIn[_base.ast.oo.Constructor] = {
    import _base.ast.any._
    import _base.ast.oo.Constructor
    class Mps(
      override val registry: ContextRegistry[_base.type, Constructor] = constructorRegistry,
      override val canToTargetLanguage: Understands[Constructor, ToTargetLanguageType[Type]] = oo.constructorCapabilities.canTranslateTypeInConstructor,
      override val canApplyType: Understands[Constructor, Apply[Type, Type, Type]] = generics.constructorCapabilities.canApplyTypeInConstructor,
    ) extends ScalaMapsIn[Constructor] {
      override def canReifyInCtxt[T]: Understands[Constructor, Reify[T, Expression]] = oo.constructorCapabilities.canReifyInConstructor[T]
    }
    new Mps()
  }

  val mapsInClasses: ScalaMapsIn[_base.ast.oo.Class] = {
    import _base.ast.any._
    import _base.ast.oo.{Class => Cls}
    class Mps(
      override val registry: ContextRegistry[_base.type, Cls] = classRegistry,
      override val canToTargetLanguage: Understands[Cls, ToTargetLanguageType[Type]] = oo.classCapabilities.canTranslateTypeInClass,
      override val canApplyType: Understands[Cls, Apply[Type, Type, Type]] = generics.classCapabilities.canApplyTypeInClass,
    ) extends ScalaMapsIn[Cls] {
      override def canReifyInCtxt[T]: Understands[Cls, Reify[T, Expression]] = new Understands[Cls, Reify[T, Expression]] {
        def perform(context: Cls, command: Reify[T, Expression]): (Cls, Expression) = {
          Command.runGenerator(context.reifyLookupMap(command.tpe)(command.value), context)
        }
      }
    }
    new Mps()
  }
}

object Maps {
  type WithBase[B <: AnyParadigm, PP <: ParametricPolymorphism.WithBase[B], OO <: OOParadigm.WithBase[B], G <: Generics.WithBase[B, OO, PP]] =
    Maps {
      val _base: B
      val parametricPolymorphism: PP
      val oo: OO
      val generics: G
    }
  
  def apply[AST <: MapsAST & BaseAST, B <: AnyParadigm.WithAST[AST]](
    base: B,
    _parametricPolymorphism: ParametricPolymorphism.WithBase[base.type],
    _oo: OOParadigm.WithBase[base.type],
    _generics: Generics.WithBase[base.type, _oo.type, _parametricPolymorphism.type],
    _methodRegistry: ContextRegistry[base.type, base.ast.any.Method],
    _constructorRegistry: ContextRegistry[base.type, base.ast.oo.Constructor],
    _classRegistry: ContextRegistry[base.type, base.ast.oo.Class],
  ): Maps.WithBase[base.type, _parametricPolymorphism.type, _oo.type, _generics.type] = {
    class Mps(
      override val _base: base.type = base,
      override val parametricPolymorphism: _parametricPolymorphism.type = _parametricPolymorphism,
      override val oo: _oo.type = _oo,
      override val generics: _generics.type = _generics,
      override val methodRegistry: ContextRegistry[base.type, _base.ast.any.Method] = _methodRegistry,
      override val constructorRegistry: ContextRegistry[base.type, _base.ast.oo.Constructor] = _constructorRegistry,
      override val classRegistry: ContextRegistry[base.type, _base.ast.oo.Class] = _classRegistry
    ) extends Maps {}
    new Mps()
  }
}
