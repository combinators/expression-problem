package org.combinators.ep.language.java.paradigm    /*DI:LD:AI*/

import com.github.javaparser.ast.`type`.{ClassOrInterfaceType, TypeParameter}
import com.github.javaparser.ast.expr.MethodCallExpr
import org.combinators.cogen.paradigm.{AddTypeParameter, Apply, GetTypeArguments, ParametricPolymorphism as PPoly, AnyParadigm as _}
import org.combinators.ep.generator.{Command, Understands}
import org.combinators.ep.generator.paradigm.*
import org.combinators.ep.language.java.Syntax.MangledName
import org.combinators.ep.language.java.TypeParamCtxt

import scala.jdk.CollectionConverters.*

trait ParametricPolymorphism[AP <: AnyParadigm] extends PPoly {
  val base: AP
  import base._
  import syntax._

  type TypeParameterContext = TypeParamCtxt

  val methodBodyCapabilities: MethodBodyCapabilities =
    new MethodBodyCapabilities {
      implicit val canAddTypeParameterInMethod: Understands[MethodBodyContext, AddTypeParameter[Name, TypeParameterContext]] =
        new Understands[MethodBodyContext, AddTypeParameter[Name, TypeParameterContext]] {
          def perform(context: MethodBodyContext, command: AddTypeParameter[MangledName, TypeParamCtxt]): (MethodBodyContext, Unit) = {
            val tpeParam = new TypeParameter()
            tpeParam.setName(command.name.toAST)
            val (resultCtxt, _) = Command.runGenerator(command.spec, TypeParamCtxt(tpeParam))

            val newMethod = context.method.clone()

            newMethod.addTypeParameter(resultCtxt.param.clone())
            (context.copy(method = newMethod), ())
          }
        }
      implicit val canGetTypeArgumentsInMethod: Understands[MethodBodyContext, GetTypeArguments[Type]] =
        new Understands[MethodBodyContext, GetTypeArguments[Type]] {
          def perform(context: MethodBodyContext, command: GetTypeArguments[Type]): (MethodBodyContext, Seq[Type]) = {
            val ctp = context.method.getTypeParameters.asScala.toSeq.map(tp => {
              val result = new ClassOrInterfaceType()
              result.setName(tp.getName)
            })
            (context, ctp)
          }
        }
      implicit val canApplyTypeInMethod: Understands[MethodBodyContext, Apply[Type, Type, Type]] =
        new Understands[MethodBodyContext, Apply[Type, Type, Type]] {
          def perform(context: MethodBodyContext, command: Apply[Type, Type, Type]): (MethodBodyContext, Type) = {
            val resultTpe = command.functional.clone().asClassOrInterfaceType()
            val boxedArguments = command.arguments.map { arg =>
              if (arg.isPrimitiveType) arg.asPrimitiveType().toBoxedType
              else arg.clone()
            }
            if (boxedArguments.nonEmpty) {
              resultTpe.setTypeArguments(boxedArguments*)
            }
            (context, resultTpe)
          }
        }
      implicit val canApplyMethodToTypeInMethod: Understands[MethodBodyContext, Apply[Expression, Type, Expression]] =
        new Understands[MethodBodyContext, Apply[Expression, Type, Expression]] {
          def perform(context: MethodBodyContext, command: Apply[Expression, Type, Expression]): (MethodBodyContext, Expression) = {
            val resultExp =
              if (command.functional.isMethodCallExpr) {
                command.functional.clone().asMethodCallExpr()
              } else if (command.functional.isFieldAccessExpr) {
                val result = new MethodCallExpr()
                val functional = command.functional.asFieldAccessExpr()
                result.setScope(functional.getScope.clone())
                result.setName(functional.getName.clone())
                result
              } else {
                new MethodCallExpr(command.functional.toString)
              }
            val boxedArguments = command.arguments.map { arg =>
              if (arg.isPrimitiveType) arg.asPrimitiveType().toBoxedType
              else arg.clone()
            }
            if (boxedArguments.nonEmpty) {
              resultExp.setTypeArguments(boxedArguments*)
            }
            (context, resultExp)
          }
        }
    }
}

object ParametricPolymorphism {
  def apply[AP <: AnyParadigm](base: AP): ParametricPolymorphism[base.type] = {
    val b: base.type = base
    new ParametricPolymorphism[b.type] {
      val base: b.type = b
    }
  }
}