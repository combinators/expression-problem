package org.combinators.ep.language.inbetween.polymorphism.generics   /*DI:LI:AI*/

import org.combinators.cogen.paradigm.{AddLowerBound, AddTypeParameter, AddUpperBound, Apply, GetCurrentTypeParameter, GetTypeArguments, AnyParadigm as AP, Generics as GS, ObjectOriented as OO, ParametricPolymorphism as PP}
import org.combinators.cogen.{Command, Understands}
import org.combinators.cogen.paradigm.ParametricPolymorphism.WithBase
import org.combinators.ep.language.inbetween.any.AnyParadigm
import org.combinators.ep.language.inbetween.oo.OOParadigm
import org.combinators.ep.language.inbetween.polymorphism.ParametricPolymorphism

trait Generics[AST <: GenericsAST, B, OO, PP](val base: AnyParadigm.WithAST[AST] & B, override val ooParadigm: OOParadigm.WithBase[AST, base.type] & OO, override val ppolyParadigm: ParametricPolymorphism.WithBase[AST, base.type] & PP) extends GS {
  import base.ast.factory
  import base.ast.polymorphismFactory
  import base.ast.ooFactory
  import base.ast.any
  import base.ast.polymorphism as pp
  import base.ast.oo

  override val classCapabilities: ClassCapabilities = new ClassCapabilities {
    override implicit val canAddTypeParameterInClass: Understands[oo.Class, AddTypeParameter[any.Name, pp.TypeParameter]] =
      new Understands[oo.Class, AddTypeParameter[any.Name, pp.TypeParameter]] {
        override def perform(context: oo.Class, command: AddTypeParameter[any.Name, pp.TypeParameter]): (oo.Class, Unit) = {
          val emptyTypeParam = polymorphismFactory.typeParameter(command.name)
          val (tpeParam, _) = Command.runGenerator(command.spec, emptyTypeParam)
          val genericClass = ooFactory.convert(context)
          (genericClass.copyAsGenericClass(typeParameters = genericClass.typeParameters :+ tpeParam), ())
        }
      }
    override implicit val canGetTypeArgumentsInClass: Understands[oo.Class, GetTypeArguments[any.Type]] =
      new Understands[oo.Class, GetTypeArguments[any.Type]] {
        override def perform(context: oo.Class, command: GetTypeArguments[any.Type]): (oo.Class, Seq[any.Type]) = {
          val genericClass = ooFactory.convert(context)
          (context, genericClass.typeParameters.map(_.toTypeArgument))
        }
      }
    override implicit val canApplyTypeInClass: Understands[oo.Class, Apply[any.Type, any.Type, any.Type]] =
      new Understands[oo.Class, Apply[any.Type, any.Type, any.Type]] {
        override def perform(context: oo.Class, command: Apply[any.Type, any.Type, any.Type]): (oo.Class, any.Type) = {
          (context, polymorphismFactory.typeApplication(command.functional, command.arguments))
        }
      }
  }
  override val typeParameterCapabilities: TypeParameterCapabilities = new TypeParameterCapabilities {
    override implicit val canGetCurrentTypeParameter: Understands[ppolyParadigm.TypeParameterContext, GetCurrentTypeParameter[any.Type]] =
      new Understands[ppolyParadigm.TypeParameterContext, GetCurrentTypeParameter[any.Type]] {
        override def perform(context: ppolyParadigm.TypeParameterContext, command: GetCurrentTypeParameter[any.Type]): (ppolyParadigm.TypeParameterContext, any.Type) = {
          (context, context.toTypeArgument)
        }
      }
    override implicit val canAddUpperBoundInTypeParameter: Understands[ppolyParadigm.TypeParameterContext, AddUpperBound[any.Type]] =
      new Understands[ppolyParadigm.TypeParameterContext, AddUpperBound[any.Type]] {
        override def perform(context: pp.TypeParameter, command: AddUpperBound[any.Type]): (pp.TypeParameter, Unit) = {
          val genericTypeParam = polymorphismFactory.convert(context)
          (genericTypeParam.copyAsTypeParameterWithBounds(upperBounds = genericTypeParam.upperBounds :+ command.bound), ())
        }
      }
    override implicit val canAddLowerBoundInTypeParameter: Understands[ppolyParadigm.TypeParameterContext, AddLowerBound[any.Type]] =
      new Understands[ppolyParadigm.TypeParameterContext, AddLowerBound[any.Type]] {
        override def perform(context: pp.TypeParameter, command: AddLowerBound[any.Type]): (pp.TypeParameter, Unit) = {
          val genericTypeParam = polymorphismFactory.convert(context)
          (genericTypeParam.copyAsTypeParameterWithBounds(lowerBounds = genericTypeParam.lowerBounds :+ command.bound), ())
        }
      }
    override implicit val canApplyTypeTypeParameter: Understands[ppolyParadigm.TypeParameterContext, Apply[any.Type, any.Type, any.Type]] =
      new Understands[ppolyParadigm.TypeParameterContext, Apply[any.Type, any.Type, any.Type]] {
        override def perform(context: ppolyParadigm.TypeParameterContext, command: Apply[any.Type, any.Type, any.Type]): (ppolyParadigm.TypeParameterContext, any.Type) = {
          (context, polymorphismFactory.typeApplication(command.functional, command.arguments))
        }
      }
  }
  override val constructorCapabilities: ConstructorCapabilities = new ConstructorCapabilities {
    override implicit val canApplyTypeInConstructor: Understands[oo.Constructor, Apply[any.Type, any.Type, any.Type]] =
      new Understands[oo.Constructor, Apply[any.Type, any.Type, any.Type]] {
        override def perform(context: oo.Constructor, command: Apply[any.Type, any.Type, any.Type]): (oo.Constructor, any.Type) = {
          (context, polymorphismFactory.typeApplication(command.functional, command.arguments))
        }
      }
    override implicit val canApplyMethodToTypeInConstructor: Understands[oo.Constructor, Apply[any.Expression, any.Type, any.Expression]] =
      new Understands[oo.Constructor, Apply[any.Expression, any.Type, any.Expression]] {
        override def perform(context: oo.Constructor, command: Apply[any.Expression, any.Type, any.Expression]): (oo.Constructor, any.Expression) = {
          (context, factory.applyExpression(command.functional, command.arguments.map(polymorphismFactory.typeReferenceExpression)))
        }
      }
  }
}

object Generics {
  type WithBase[AST <: GenericsAST, B <: AnyParadigm.WithAST[AST], OO <: OOParadigm.WithBase[AST, B], PP <: ParametricPolymorphism.WithBase[AST, B]] = Generics[AST, B, OO, PP] {}
  def apply[AST <: GenericsAST, B <: AnyParadigm.WithAST[AST], OO <: OOParadigm.WithBase[AST, B], PP <: ParametricPolymorphism.WithBase[AST, B]](_base: B, _ooParadigm: OO & OOParadigm.WithBase[AST, _base.type], _ppolyParadigm: PP & ParametricPolymorphism.WithBase[AST, _base.type]): WithBase[AST, B, OO, PP] = new Generics[AST, B, OO, PP](_base, _ooParadigm, _ppolyParadigm) {}
}
