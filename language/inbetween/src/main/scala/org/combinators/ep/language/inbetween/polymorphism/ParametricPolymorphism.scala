package org.combinators.ep.language.inbetween.polymorphism   /*DI:LI:AI*/


import org.combinators.cogen.paradigm.{AddTypeParameter, Apply, GetTypeArguments, ParametricPolymorphism as PP}
import org.combinators.cogen.{Command, Understands}
import org.combinators.ep.language.inbetween.any.AnyParadigm
import org.combinators.ep.language.inbetween.any


trait ParametricPolymorphism[FT <: FinalTypes, FactoryType <: Factory[FT]] extends PP {
  val base: AnyParadigm.WithFT[FT, FactoryType]
  import base.{FT=>_, _}
  import syntax._
  lazy val factory: base.factory.type = base.factory
  override type TypeParameterContext = TypeParameter[FT]
  override val methodBodyCapabilities: MethodBodyCapabilities = new MethodBodyCapabilities {
    override implicit val canAddTypeParameterInMethod: Understands[any.Method[FT], AddTypeParameter[any.Name[FT], TypeParameterContext]] =
      new Understands[any.Method[FT], AddTypeParameter[any.Name[FT], TypeParameterContext]] {
        override def perform(context: any.Method[FT], command: AddTypeParameter[any.Name[FT], TypeParameter[FT]]): (any.Method[FT], Unit) = {
          val emptyTypeParam = factory.typeParameter(command.name)
          val (tpeParam, _) = Command.runGenerator(command.spec, emptyTypeParam)
          val currentMethod = factory.convert(context)
          val withTypeParameter = currentMethod.copyAsTypeParamMethod(typeParameters = currentMethod.typeParameters :+ tpeParam)
          (withTypeParameter, ())
        }
      }
    override implicit val canGetTypeArgumentsInMethod: Understands[any.Method[FT], GetTypeArguments[any.Type[FT]]] =
      new Understands[any.Method[FT], GetTypeArguments[any.Type[FT]]] {
        override def perform(context: any.Method[FT], command: GetTypeArguments[any.Type[FT]]): (any.Method[FT], Seq[any.Type[FT]]) = {
          (context, factory.convert(context).typeParameters.map(_.toTypeArgument))
        }
      }
    override implicit val canApplyTypeInMethod: Understands[any.Method[FT], Apply[any.Type[FT], any.Type[FT], any.Type[FT]]] =
      new Understands[any.Method[FT], Apply[any.Type[FT], any.Type[FT], any.Type[FT]]] {
        override def perform(context: any.Method[FT], command: Apply[any.Type[FT], any.Type[FT], any.Type[FT]]): (any.Method[FT], any.Type[FT]) = {
          (context, factory.typeApplication(command.functional, command.arguments))
        }
      }
    override implicit val canApplyMethodToTypeInMethod: Understands[any.Method[FT], Apply[any.Expression[FT], any.Type[FT], any.Expression[FT]]] =
      new Understands[any.Method[FT], Apply[any.Expression[FT], any.Type[FT], any.Expression[FT]]] {
        override def perform(context: any.Method[FT], command: Apply[any.Expression[FT], any.Type[FT], any.Expression[FT]]): (any.Method[FT], any.Expression[FT]) = {
          (context, factory.applyExpression(command.functional, command.arguments.map(factory.typeReferenceExpression)))
        }
      }
  }
}

object ParametricPolymorphism {
  type WithBase[FT <: FinalTypes, FactoryType <: Factory[FT], B <: AnyParadigm.WithFT[FT, FactoryType]] = ParametricPolymorphism[FT, FactoryType] { val base: B }
  def apply[FT <: FinalTypes, FactoryType <: Factory[FT], B <: AnyParadigm.WithFT[FT, FactoryType]](_base: B): WithBase[FT, FactoryType, _base.type] = new ParametricPolymorphism[FT, FactoryType] {
    val base: _base.type = _base
  }
}