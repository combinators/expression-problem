package org.combinators.ep.language.inbetween.polymorphism.generics   /*DI:LI:AI*/

import org.combinators.cogen.paradigm.{AddLowerBound, AddTypeParameter, AddUpperBound, Apply, GetCurrentTypeParameter, GetTypeArguments, AnyParadigm as AP, Generics as GS, ObjectOriented as OO, ParametricPolymorphism as PP}
import org.combinators.cogen.{Command, Understands}
import org.combinators.cogen.paradigm.ParametricPolymorphism.WithBase
import org.combinators.ep.language.inbetween.any.AnyParadigm2
import org.combinators.ep.language.inbetween.oo.OOParadigm2
import org.combinators.ep.language.inbetween.polymorphism.ParametricPolymorphism2

trait Generics2(val base: AnyParadigm2.WithAST[GenericsAST], override val ooParadigm: OOParadigm2.WithBase[base.ast.type, base.type], override val ppolyParadigm: ParametricPolymorphism2.WithBase[base.ast.type, base.type]) extends GS {
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

object Generics2 {
  type WithBase[AST <: GenericsAST, B <: AnyParadigm2.WithAST[AST], OO[A <: GenericsAST, OOB <: AnyParadigm2.WithAST[A]] <: OOParadigm2.WithBase[A, OOB], PP[A <: GenericsAST, PPB <: AnyParadigm2.WithAST[A]] <: ParametricPolymorphism2.WithBase[A, PPB]] = Generics2 {val base: B; val ooParadigm: OO[base.ast.type, base.type]; val ppolyParadigm: PP[base.ast.type, base.type]}

  trait WB[AST <: GenericsAST, B <: AnyParadigm2.WithAST[AST], OO[A <: GenericsAST, OOB <: AnyParadigm2.WithAST[A]] <: OOParadigm2.WithBase[A, OOB], PP[A <: GenericsAST, PPB <: AnyParadigm2.WithAST[A]] <: ParametricPolymorphism2.WithBase[A, PPB]](override val base: B, override val ooParadigm: OO[base.ast.type, base.type], override val ppolyParadigm: PP[base.ast.type, base.type]) extends Generics2 {}

  def apply[AST <: GenericsAST, B <: AnyParadigm2.WithAST[AST], OO[A <: GenericsAST, OOB <: AnyParadigm2.WithAST[A]] <: OOParadigm2.WithBase[A, OOB], PP[A <: GenericsAST, PPB <: AnyParadigm2.WithAST[A]] <: ParametricPolymorphism2.WithBase[A, PPB]](_base: B, _ooParadigm: OO[_base.ast.type, _base.type], _ppolyParadigm: PP[_base.ast.type, _base.type]): WithBase[AST, B, OO, PP] = new WB[AST, _base.type, OO, PP](_base, _ooParadigm, _ppolyParadigm) with Generics2(_base, _ooParadigm, _ppolyParadigm) {}
}
