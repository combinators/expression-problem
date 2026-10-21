package org.combinators.ep.language.scala.ast.ffi

import org.combinators.cogen.Command.Generator
import org.combinators.cogen.paradigm.AnyParadigm.syntax.forEach
import org.combinators.cogen.paradigm.{Apply, InstantiateObject, Reify, ToTargetLanguageType}
import org.combinators.cogen.{Command, InstanceRep, TypeRep, Understands}
import org.combinators.ep.language.inbetween.{ContextRegistry, ffi}
import org.combinators.ep.language.inbetween.any.AnyParadigm
import org.combinators.ep.language.scala.ast.BaseAST
import org.combinators.equals.ffi.inbetween.BaseType as Bts
import org.combinators.ep.language.inbetween.oo.OOParadigm
import org.combinators.ep.language.inbetween.polymorphism.ParametricPolymorphism
import org.combinators.ep.language.inbetween.polymorphism.generics.Generics
import org.combinators.equals.ffi.inbetween.BaseTypeAST
import org.combinators.equals.ffi.BaseType as Bt

import scala.reflect.{ClassTag, classTag}

trait BaseType extends Bts {
  override val _base: AnyParadigm { val ast: BaseTypeAST & BaseAST }
  val oo: OOParadigm.WithBase[_base.type]

  val nameProvider: _base.ast.nameProvider.ScalaNameProvider = _base.ast.nameProviderFactory.scalaNameProvider

  val methodRegistry: ContextRegistry[_base.type, _base.ast.any.Method]
  val constructorRegistry: ContextRegistry[_base.type, _base.ast.oo.Constructor]
  val classRegistry: ContextRegistry[_base.type, _base.ast.oo.Class]

  trait ScalaBaseTypeIn[Ctxt] extends super.BaseTypeIn[Ctxt] {
    import _base.ast
    val canToTargetLanguage: Understands[Ctxt, ToTargetLanguageType[ast.any.Type]]
     
    def canReifyInCtxt[T]: Understands[Ctxt, Reify[T, ast.any.Expression]]

    override val tpeLookup: TypeRep => Option[Generator[Ctxt, _base.syntax.Type]] = {
      case Bt.AnyTpe => 
        val result = _base.ast.baseTypeOpsFactory.baseType()
        Some(Command.lift(result))

      case Bt.CompositeTpe(args) =>
        val result = _base.ast.baseTypeOpsFactory.compositeType(args)
        Some(Command.lift(result))
        
      case _ => None
    }

    override val reifylookup: (tpeRep: TypeRep) => tpeRep.HostType => Option[Generator[Ctxt, _base.syntax.Expression]] = {
      case Bt.AnyTpe =>
        _ => None

      case compTpe@Bt.CompositeTpe(args) => 
        elems => {
          val result: Generator[Ctxt, _base.syntax.Expression] = for {
            reifiedElems <- forEach(elems.asInstanceOf[Seq[(String, InstanceRep)]].toSeq) { case (name, ir) =>
              for {
                value <- Reify(ir.tpe, ir.inst).interpret(using canReifyInCtxt)
              } yield value
            }
            
            tpe <- ToTargetLanguageType(compTpe).interpret(using canToTargetLanguage)
            obj = _base.ast.ooFactory.objectInstantiationExpression(tpe, reifiedElems, None)
          } yield obj
          
          Some(result)
        }

      case _ => _ => None
    }
  }

  val baseTypeInMethods: ScalaBaseTypeIn[_base.ast.any.Method] = {
    import _base.ast.any._
    class Bts(
         override val registry: ContextRegistry[_base.type, Method] = methodRegistry,
         override val canToTargetLanguage: Understands[Method, ToTargetLanguageType[Type]] = _base.methodBodyCapabilities.canTransformTypeInMethodBody,
        ) extends ScalaBaseTypeIn[Method] {
      override def canReifyInCtxt[T]: Understands[Method, Reify[T, Expression]] = _base.methodBodyCapabilities.canReifyInMethodBody
    }
    new Bts()
  }
  
  val baseTypeInConstructors: ScalaBaseTypeIn[_base.ast.oo.Constructor] = {
    import _base.ast.any.Expression
    import _base.ast.oo._
    class Bts(
         override val registry: ContextRegistry[_base.type, Constructor] = constructorRegistry,
         override val canToTargetLanguage: Understands[Constructor, ToTargetLanguageType[_base.ast.any.Type]] = oo.constructorCapabilities.canTranslateTypeInConstructor,
       ) extends ScalaBaseTypeIn[Constructor] {
      override def canReifyInCtxt[T]: Understands[Constructor, Reify[T, Expression]] = oo.constructorCapabilities.canReifyInConstructor
    }
    new Bts()
  }
  
  val baseTypeInClasses: ScalaBaseTypeIn[_base.ast.oo.Class] = {
    import _base.ast.any.Expression
    import _base.ast.oo._
    class Bts(
         override val registry: ContextRegistry[_base.type, Class] = classRegistry,
         override val canToTargetLanguage: Understands[Class, ToTargetLanguageType[_base.ast.any.Type]] = oo.classCapabilities.canTranslateTypeInClass
       ) extends ScalaBaseTypeIn[Class] {
      override def canReifyInCtxt[T]: Understands[Class, Reify[T, Expression]] = new Understands[Class, Reify[T, Expression]] {
        def perform(context: Class, command: Reify[T, Expression]): (Class, Expression) = {
          Command.runGenerator(context.reifyLookupMap(command.tpe)(command.value), context)
        }
      }
    }
    new Bts()
  }
}

object BaseType {
  type WithBase[B <: AnyParadigm, OO <: OOParadigm.WithBase[B]] =
    BaseType {
      val _base: B
      val oo: OO
    }

  def apply[AST <: BaseTypeAST & BaseAST, B <: AnyParadigm.WithAST[AST]](
          base: B,
          _oo: OOParadigm.WithBase[base.type],
          _methodRegistry: ContextRegistry[base.type, base.ast.any.Method],
          _constructorRegistry: ContextRegistry[base.type, base.ast.oo.Constructor],
          _classRegistry: ContextRegistry[base.type, base.ast.oo.Class],
        ): BaseType.WithBase[base.type, _oo.type] = {
    class Bts(
               override val _base: base.type = base,
               override val oo: _oo.type = _oo,
               override val methodRegistry: ContextRegistry[base.type, _base.ast.any.Method] = _methodRegistry,
               override val constructorRegistry: ContextRegistry[base.type, _base.ast.oo.Constructor] = _constructorRegistry,
               override val classRegistry: ContextRegistry[base.type, _base.ast.oo.Class] = _classRegistry
             ) extends BaseType {}
    new Bts()
  }
}
