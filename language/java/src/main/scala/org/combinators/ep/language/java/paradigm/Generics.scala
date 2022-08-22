package org.combinators.ep.language.java.paradigm    /*DI:LD:AI*/

import com.github.javaparser.ast.`type`.TypeParameter
import org.combinators.ep.generator.{Command, Understands}
import org.combinators.ep.generator.paradigm.{Generics => Gen, AnyParadigm => _, ObjectOriented => _, _}
import org.combinators.ep.language.java.TypeParamCtxt

import scala.jdk.CollectionConverters._

trait Generics[AP <: AnyParadigm] extends Gen {
  val base: AP
  val ooParadigm: ObjectOriented[base.type]
  val ppolyParadigm: ParametricPolymorphism[base.type]

  import base.syntax._
  import ooParadigm._
  import ppolyParadigm._
  val classCapabilities: ClassCapabilities =
    new ClassCapabilities {
      implicit val canAddTypeParameterInClass: Understands[ClassContext, AddTypeParameter[Name, TypeParameterContext]] =
        new Understands[ClassContext, AddTypeParameter[Name, TypeParameterContext]] {
          def perform(context: ClassContext, command: AddTypeParameter[Name, TypeParameterContext]): (ClassContext, Unit) = {
            val (resultCtxt, _) = Command.runGenerator(command.spec, TypeParamCtxt(new TypeParameter()))
            val tpeParam = resultCtxt.param.clone()
            tpeParam.setName(command.name.toAST)
            val newCls = context.cls.clone()
            newCls.addTypeParameter(tpeParam)
            (context.copy(cls = newCls), ())
          }
        }
      implicit val canGetTypeArgumentsInClass: Understands[ClassContext, GetTypeArguments[Type]] =
        new Understands[ClassContext, GetTypeArguments[Type]] {
          def perform(context: ClassContext, command: GetTypeArguments[Type]): (ClassContext, Seq[Type]) = {
            (context, context.cls.getTypeParameters.asScala)
          }
        }
      implicit val canApplyTypeInClass: Understands[ClassContext, Apply[Type, Type, Type]] =
        new Understands[ClassContext, Apply[Type, Type, Type]] {
          def perform(context: ClassContext, command: Apply[Type, Type, Type]): (ClassContext, Type) = {
            val resultTpe = command.functional.clone().asClassOrInterfaceType()
            val boxedArguments = command.arguments.map { arg =>
              if (arg.isPrimitiveType) arg.asPrimitiveType().toBoxedType
              else arg.clone()
            }
            if (boxedArguments.nonEmpty) {
              resultTpe.setTypeArguments(boxedArguments: _*)
            }
            (context, resultTpe)
          }
        }
    }
  val typeParameterCapabilities: TypeParameterCapabilities =
    new TypeParameterCapabilities {
      implicit val canAddUpperBoundInTypeParameter: Understands[TypeParameterContext, AddUpperBound[Type]] =
        new Understands[TypeParameterContext, AddUpperBound[Type]] {
          def perform(context: TypeParameterContext, command: AddUpperBound[Type]): (TypeParameterContext, Unit) = {
            throw new UnsupportedOperationException("Sorry, Java does not support upper bounds on type parameters.")
          }
        }
      implicit val canAddLowerBoundInTypeParameter: Understands[TypeParameterContext, AddLowerBound[Type]] =
        new Understands[TypeParameterContext, AddLowerBound[Type]] {
          def perform(context: TypeParameterContext, command: AddLowerBound[Type]): (TypeParameterContext, Unit) = {
            val newParam = context.param.clone()
            newParam.getTypeBound.add(command.bound.toClassOrInterfaceType().get().clone())
            (context.copy(param = newParam), ())
          }
        }
      implicit val canApplyTypeTypeParameter: Understands[TypeParameterContext, Apply[Type, Type, Type]] =
        new Understands[TypeParameterContext, Apply[Type, Type, Type]] {
          def perform(context: TypeParameterContext, command: Apply[Type, Type, Type]): (TypeParameterContext, Type) = {
            val resultTpe = command.functional.clone().asClassOrInterfaceType()
            val boxedArguments = command.arguments.map { arg =>
              if (arg.isPrimitiveType) arg.asPrimitiveType().toBoxedType
              else arg.clone()
            }
            if (boxedArguments.nonEmpty) {
              resultTpe.setTypeArguments(boxedArguments: _*)
            }
            (context, resultTpe)
          }
        }
    }
  val constructorCapabilities: ConstructorCapabilities =
    new ConstructorCapabilities {
      implicit val canApplyTypeInConstructor: Understands[ConstructorContext, Apply[Type, Type, Type]] =
        new Understands[ConstructorContext, Apply[Type, Type, Type]] {
          def perform(context: ConstructorContext, command: Apply[Type, Type, Type]): (ConstructorContext, Type) = {
            val resultTpe = command.functional.clone().asClassOrInterfaceType()
            val boxedArguments = command.arguments.map { arg =>
              if (arg.isPrimitiveType) arg.asPrimitiveType().toBoxedType
              else arg.clone()
            }
            if (boxedArguments.nonEmpty) {
              resultTpe.setTypeArguments(boxedArguments: _*)
            }
            (context, resultTpe)
          }
        }
      implicit val canApplyMethodToTypeInConstructor: Understands[ConstructorContext, Apply[Expression, Type, Expression]] =
        new Understands[ConstructorContext, Apply[Expression, Type, Expression]] {
          def perform(context: ConstructorContext, command: Apply[Expression, Type, Expression]): (ConstructorContext, Expression) = {
            val resultExp = command.functional.clone().asMethodReferenceExpr()
            val boxedArguments = command.arguments.map { arg =>
              if (arg.isPrimitiveType) arg.asPrimitiveType().toBoxedType
              else arg.clone()
            }
            if (boxedArguments.nonEmpty) {
              resultExp.setTypeArguments(boxedArguments: _*)
            }
            (context, resultExp)
          }
        }
    }
}

object Generics {

  type Aux[AP <: AnyParadigm, OO <: ObjectOriented[AP], PP <: ParametricPolymorphism[AP]] = Generics[AP] {
    val base: AP
    val ooParadigm: OO
    val ppolyParadigm: PP
  }

  def apply[AP <: AnyParadigm](base: AP)(ooParadigm: ObjectOriented[base.type], ppolyParadigm: ParametricPolymorphism[base.type]): Aux[base.type, ooParadigm.type, ppolyParadigm.type] = {
    val b: base.type = base
    val oo: ooParadigm.type = ooParadigm
    val ppol: ppolyParadigm.type = ppolyParadigm

    new Generics[b.type] {
      val base: b.type = b
      val ooParadigm: oo.type = oo
      val ppolyParadigm: ppol.type = ppol
    }
  }
}