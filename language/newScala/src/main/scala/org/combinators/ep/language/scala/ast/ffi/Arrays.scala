package org.combinators.ep.language.scala.ast.ffi

import org.combinators.cogen.Command.Generator
import org.combinators.cogen.paradigm.AnyParadigm.syntax.forEach
import org.combinators.cogen.paradigm.ffi.CreateArray
import org.combinators.cogen.paradigm.{Apply, Reify, ToTargetLanguageType}
import org.combinators.cogen.{Command, InstanceRep, TypeRep, Understands}
import org.combinators.ep.language.inbetween.ContextRegistry
import org.combinators.ep.language.inbetween.any.AnyParadigm
import org.combinators.ep.language.inbetween.ffi.Arrays as Ar
import org.combinators.ep.language.inbetween.oo.OOParadigm
import org.combinators.ep.language.inbetween.polymorphism.ParametricPolymorphism
import org.combinators.ep.language.inbetween.polymorphism.generics.Generics
import org.combinators.ep.language.scala.ast.BaseAST

import scala.reflect.{ClassTag, classTag}

trait Arrays[AST <: ArraysAST & BaseAST, B <: AnyParadigm.WithAST[AST]] extends Ar[AST, B] {
  val parametricPolymorphism: ParametricPolymorphism.WithBase[_base.ast.type, _base.type]
  val oo: OOParadigm[_base.ast.type, _base.type]
  val generics: Generics.WithBase[_base.ast.type, _base.type, oo.type, parametricPolymorphism.type]
  
  val methodRegistry: ContextRegistry[_base.type, _base.ast.any.Method]
  val constructorRegistry: ContextRegistry[_base.type, _base.ast.oo.Constructor]
  val classRegistry: ContextRegistry[_base.type, _base.ast.oo.Class]

  trait ScalaArraysIn[Ctxt] extends super.ArraysIn[Ctxt] {
    import _base.ast
    val canToTargetLanguage: Understands[Ctxt, ToTargetLanguageType[ast.any.Type]]
    val canApplyType: Understands[Ctxt, Apply[ast.any.Type, ast.any.Type, ast.any.Type]]
    def canReifyInCtxt[T]: Understands[Ctxt, Reify[T, ast.any.Expression]]

    override val tpeLookup: TypeRep => Option[Generator[Ctxt, _base.syntax.Type]] = {
      case TypeRep.Array(elemTpe) => Some(
        for {
          elemTpe <- ToTargetLanguageType[ast.any.Type](elemTpe).interpret(using canToTargetLanguage)
          arrayTpe <- Command.lift(ast.arraysOpsFactory.array())
          tpe <- Apply[
            ast.any.Type,
            ast.any.Type,
            ast.any.Type](arrayTpe, Seq(elemTpe)).interpret(using canApplyType)
        } yield tpe)
      case _ => None
    }
    
    override val reifylookup: (tpeRep: TypeRep) => tpeRep.HostType => Option[Generator[Ctxt, _base.syntax.Expression]] = {
      tpe => arr => {
        tpe match {
          case TypeRep.Array(elemTpe) => {
            import _base.syntax.*
            import arrayCapabilities.canCreate
            def elements(elemTypeRep:TypeRep)(elem:elemTypeRep.HostType) : Generator[Ctxt, Seq[Expression]] = {
              elemTypeRep match {
                case TypeRep.Array(innerElemTypeRep) => {
                  val seq_gen = elem.asInstanceOf[Array[innerElemTypeRep.HostType]].map(innerElem => elements(innerElemTypeRep)(innerElem))
                  for {
                    flattened <- seq_gen.foldLeft(Command.lift[Ctxt,Seq[Expression]](Seq.empty[Expression])){ case (acc, next_gen) =>
                      for {
                        acc_result <- acc
                        next_result <- next_gen
                      } yield acc_result ++ next_result
                    }
                  } yield flattened
                }
                // recursively translates innermost elements
                case _ => Reify[elemTypeRep.HostType, Expression](elemTypeRep, elem).interpret(using canReifyInCtxt).map(Seq(_))
              }
            }

            def elementType(elemTypeRep: TypeRep)(elem: elemTypeRep.HostType): Generator[Ctxt, Type] = {
              elemTypeRep match {
                case TypeRep.Array(innerElemTypeRep) =>
                  elementType(innerElemTypeRep)(elem.asInstanceOf[Array[innerElemTypeRep.HostType]].head)

                // recursively find type of innermost element
                case _ => for {
                  elemType <- ToTargetLanguageType[Type](elemTypeRep).interpret(using canToTargetLanguage)
                } yield elemType
              }
            }

            // helper function to get flattened elements
            def dimensions(elemTypeRep: TypeRep)(elem: elemTypeRep.HostType): Seq[Int] = {
              elemTypeRep match {
                case TypeRep.Array(innerElemTypeRep) => {
                  val outer = elem.asInstanceOf[Array[elemTypeRep.HostType]].length

                  // inner arrays must be uniform length
                  val inner = dimensions(innerElemTypeRep)(elem.asInstanceOf[Array[innerElemTypeRep.HostType]].head)
                  outer +: inner
                }

                // recursively translates innermost elements
                case _ => Seq()
              }
            }

            val result = for {
              elems <- elements(tpe)(arr)
              dims <- forEach(dimensions(tpe)(arr)) { dim =>
                Reify[Int, Expression](TypeRep.Int, dim).interpret(using canReifyInCtxt)
              }
              elemType <- elementType(tpe)(arr)
              res <- CreateArray[Type, Expression](elemType, dims, Some(dimensions(tpe)(arr), elems)).interpret(using canCreate)
            } yield res
            Some(result)
          }
          case _ => None
        }
      }
     
    }
  }
  
  val arraysInMethods: ScalaArraysIn[_base.ast.any.Method] = {
    import _base.ast.any.*
    class ArraysInMethods(
      override val registry: ContextRegistry[_base.type, Method] = methodRegistry,
      override val canToTargetLanguage: Understands[Method, ToTargetLanguageType[Type]] = _base.methodBodyCapabilities.canTransformTypeInMethodBody,
      override val canApplyType: Understands[Method, Apply[Type, Type, Type]] = parametricPolymorphism.methodBodyCapabilities.canApplyTypeInMethod,
    ) extends ScalaArraysIn[Method] {
      override def canReifyInCtxt[T]: Understands[Method, Reify[T, Expression]] = _base.methodBodyCapabilities.canReifyInMethodBody
    }
    new ArraysInMethods()
  }
  val arraysInConstructors: ScalaArraysIn[_base.ast.oo.Constructor] = {
    import _base.ast.any.*
    import _base.ast.oo.Constructor
    class ArraysInConstructors(
      override val registry: ContextRegistry[_base.type, Constructor] = constructorRegistry,
      override val canToTargetLanguage: Understands[Constructor, ToTargetLanguageType[Type]] = oo.constructorCapabilities.canTranslateTypeInConstructor,
      override val canApplyType: Understands[Constructor, Apply[Type, Type, Type]] = generics.constructorCapabilities.canApplyTypeInConstructor,
    ) extends ScalaArraysIn[Constructor] {
      override def canReifyInCtxt[T]: Understands[Constructor, Reify[T, Expression]] = oo.constructorCapabilities.canReifyInConstructor[T]
    }
    new ArraysInConstructors()
  }
  
  val arraysInClasses: ScalaArraysIn[_base.ast.oo.Class] = {
    import _base.ast.any.*
    import _base.ast.oo.Class as Cls
    class ArraysInClasses(
      override val registry: ContextRegistry[_base.type, Cls] = classRegistry,
      override val canToTargetLanguage: Understands[Cls, ToTargetLanguageType[Type]] = oo.classCapabilities.canTranslateTypeInClass,
      override val canApplyType: Understands[Cls, Apply[Type, Type, Type]] = generics.classCapabilities.canApplyTypeInClass,
    ) extends ScalaArraysIn[Cls] {
      override def canReifyInCtxt[T]: Understands[Cls, Reify[T, Expression]] = new Understands[Cls, Reify[T, Expression]] {
        def perform(context: Cls, command: Reify[T, Expression]): (Cls, Expression) = {
          Command.runGenerator(context.reifyLookupMap(command.tpe)(command.value), context)
        }
      }
    }
    new ArraysInClasses()
  }
}

object Arrays {
  def apply[AST <: ArraysAST & BaseAST, B <: AnyParadigm.WithAST[AST]](
    base: B,
    parametricPolymorphism: ParametricPolymorphism.WithBase[base.ast.type, base.type],
    oo: OOParadigm[base.ast.type, base.type],
    generics: Generics.WithBase[base.ast.type, base.type, oo.type, parametricPolymorphism.type],
    methodRegistry: ContextRegistry[base.type, base.ast.any.Method],
    constructorRegistry: ContextRegistry[base.type, base.ast.oo.Constructor],
    classRegistry: ContextRegistry[base.type, base.ast.oo.Class],
  ): Arrays[base.ast.type, base.type] = {
    class Arrs(
      override val _base: base.type,
      override val parametricPolymorphism: ParametricPolymorphism.WithBase[_base.ast.type, _base.type],
      override val oo: OOParadigm[_base.ast.type, _base.type],
      override val generics: Generics.WithBase[_base.ast.type, _base.type, oo.type, parametricPolymorphism.type],
      override val methodRegistry: ContextRegistry[base.type, _base.ast.any.Method],
      override val constructorRegistry: ContextRegistry[base.type, _base.ast.oo.Constructor],
      override val classRegistry: ContextRegistry[base.type, _base.ast.oo.Class]
    ) extends Arrays[_base.ast.type, _base.type] {}
    new Arrs(base, parametricPolymorphism, oo, generics, methodRegistry, constructorRegistry, classRegistry)
  }
}
