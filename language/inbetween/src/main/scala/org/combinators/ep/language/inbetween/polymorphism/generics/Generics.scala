package org.combinators.ep.language.inbetween.polymorphism.generics   /*DI:LI:AI*/

import org.combinators.cogen.paradigm.{AddLowerBound, AddTypeParameter, AddUpperBound, Apply, GetCurrentTypeParameter, GetTypeArguments, AnyParadigm as AP, Generics as GS, ObjectOriented as OO, ParametricPolymorphism as PP}
import org.combinators.ep.generator.{Command, Understands}
import org.combinators.cogen.paradigm.ParametricPolymorphism.WithBase
import org.combinators.ep.language.inbetween.{any, oo, polymorphism}
import org.combinators.ep.language.inbetween.any.AnyParadigm
import org.combinators.ep.language.inbetween.oo.OOParadigm
import org.combinators.ep.language.inbetween.polymorphism.ParametricPolymorphism

trait Generics[FT <: FinalTypes, FactoryType <: Factory[FT]] extends GS {
  val base: AnyParadigm.WithFT[FT, FactoryType]
  import base.{FT=>_, FactoryType=>_, _}
  import syntax._
  val ooParadigm: OOParadigm.WithBase[FT, FactoryType, base.type]
  val ppolyParadigm: ParametricPolymorphism.WithBase[FT, FactoryType, base.type]
  lazy val factory: base.factory.type = base.factory
  override val classCapabilities: ClassCapabilities = new ClassCapabilities {
    override implicit val canAddTypeParameterInClass: Understands[oo.Class[FT], AddTypeParameter[any.Name[FT], polymorphism.TypeParameter[FT]]] =
      new Understands[oo.Class[FT], AddTypeParameter[any.Name[FT], polymorphism.TypeParameter[FT]]] {
        override def perform(context: oo.Class[FT], command: AddTypeParameter[any.Name[FT], polymorphism.TypeParameter[FT]]): (oo.Class[FT], Unit) = {
          val emptyTypeParam = factory.typeParameter(command.name)
          val (tpeParam, _) = Command.runGenerator(command.spec, emptyTypeParam)
          val genericClass = factory.convert(context)
          (genericClass.copyAsGenericClass(typeParameters = genericClass.typeParameters :+ tpeParam), ())
        }
      }
    override implicit val canGetTypeArgumentsInClass: Understands[oo.Class[FT], GetTypeArguments[any.Type[FT]]] =
      new Understands[oo.Class[FT], GetTypeArguments[any.Type[FT]]] {
        override def perform(context: oo.Class[FT], command: GetTypeArguments[any.Type[FT]]): (oo.Class[FT], Seq[any.Type[FT]]) = {
          val genericClass = factory.convert(context)
          (context, genericClass.typeParameters.map(_.toTypeArgument))
        }
      }
    override implicit val canApplyTypeInClass: Understands[oo.Class[FT], Apply[any.Type[FT], any.Type[FT], any.Type[FT]]] =
      new Understands[oo.Class[FT], Apply[any.Type[FT], any.Type[FT], any.Type[FT]]] {
        override def perform(context: oo.Class[FT], command: Apply[any.Type[FT], any.Type[FT], any.Type[FT]]): (oo.Class[FT], any.Type[FT]) = {
          (context, factory.typeApplication(command.functional, command.arguments))
        }
      }
  }
  override val typeParameterCapabilities: TypeParameterCapabilities = new TypeParameterCapabilities {
    override implicit val canGetCurrentTypeParameter: Understands[ppolyParadigm.TypeParameterContext, GetCurrentTypeParameter[any.Type[FT]]] =
      new Understands[ppolyParadigm.TypeParameterContext, GetCurrentTypeParameter[any.Type[FT]]] {
        override def perform(context: ppolyParadigm.TypeParameterContext, command: GetCurrentTypeParameter[any.Type[FT]]): (ppolyParadigm.TypeParameterContext, any.Type[FT]) = {
          (context, context.toTypeArgument)
        }
      }
    override implicit val canAddUpperBoundInTypeParameter: Understands[ppolyParadigm.TypeParameterContext, AddUpperBound[any.Type[FT]]] =
      new Understands[ppolyParadigm.TypeParameterContext, AddUpperBound[any.Type[FT]]] {
        override def perform(context: polymorphism.TypeParameter[FT], command: AddUpperBound[any.Type[FT]]): (polymorphism.TypeParameter[FT], Unit) = {
          val genericTypeParam = factory.convert(context)
          (genericTypeParam.copyAsTypeParameterWithBounds(upperBounds = genericTypeParam.upperBounds :+ command.bound), ())
        }
      }
    override implicit val canAddLowerBoundInTypeParameter: Understands[ppolyParadigm.TypeParameterContext, AddLowerBound[any.Type[FT]]] =
      new Understands[ppolyParadigm.TypeParameterContext, AddLowerBound[any.Type[FT]]] {
        override def perform(context: polymorphism.TypeParameter[FT], command: AddLowerBound[any.Type[FT]]): (polymorphism.TypeParameter[FT], Unit) = {
          val genericTypeParam = factory.convert(context)
          (genericTypeParam.copyAsTypeParameterWithBounds(lowerBounds = genericTypeParam.lowerBounds :+ command.bound), ())
        }
      }
    override implicit val canApplyTypeTypeParameter: Understands[ppolyParadigm.TypeParameterContext, Apply[any.Type[FT], any.Type[FT], any.Type[FT]]] =
      new Understands[ppolyParadigm.TypeParameterContext, Apply[any.Type[FT], any.Type[FT], any.Type[FT]]] {
        override def perform(context: ppolyParadigm.TypeParameterContext, command: Apply[any.Type[FT], any.Type[FT], any.Type[FT]]): (ppolyParadigm.TypeParameterContext, any.Type[FT]) = {
          (context, factory.typeApplication(command.functional, command.arguments))
        }
      }
  }
  override val constructorCapabilities: ConstructorCapabilities = new ConstructorCapabilities {
    override implicit val canApplyTypeInConstructor: Understands[oo.Constructor[FT], Apply[any.Type[FT], any.Type[FT], any.Type[FT]]] =
      new Understands[oo.Constructor[FT], Apply[any.Type[FT], any.Type[FT], any.Type[FT]]] {
        override def perform(context: oo.Constructor[FT], command: Apply[any.Type[FT], any.Type[FT], any.Type[FT]]): (oo.Constructor[FT], any.Type[FT]) = {
          (context, factory.typeApplication(command.functional, command.arguments))
        }
      }
    override implicit val canApplyMethodToTypeInConstructor: Understands[oo.Constructor[FT], Apply[any.Expression[FT], any.Type[FT], any.Expression[FT]]] =
      new Understands[oo.Constructor[FT], Apply[any.Expression[FT], any.Type[FT], any.Expression[FT]]] {
        override def perform(context: oo.Constructor[FT], command: Apply[any.Expression[FT], any.Type[FT], any.Expression[FT]]): (oo.Constructor[FT], any.Expression[FT]) = {
          (context, factory.applyExpression(command.functional, command.arguments.map(factory.typeReferenceExpression)))
        }
      }
  }
}

object Generics {
  type WithBase[FT <: FinalTypes, FactoryType <: Factory[FT], B <: AnyParadigm.WithFT[FT, FactoryType], OO <: OOParadigm.WithBase[FT, FactoryType, B], PP <: ParametricPolymorphism.WithBase[FT, FactoryType, B]] =
    Generics[FT, FactoryType] { val base: B; val ooParadigm: OO; val ppolyParadigm: PP }
  def apply[FT <: FinalTypes, FactoryType <: Factory[FT], B <: AnyParadigm.WithFT[FT, FactoryType]](_base: B)(oo: OOParadigm.WithBase[FT, FactoryType, _base.type], pp: ParametricPolymorphism.WithBase[FT, FactoryType, _base.type]): WithBase[FT, FactoryType, _base.type, oo.type, pp.type] =
    new Generics[FT, FactoryType] {
      val base: _base.type = _base
      val ooParadigm: oo.type = oo
      val ppolyParadigm: pp.type = pp
    }
}