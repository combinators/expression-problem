package org.combinators.ep.language.inbetween.polymorphism

/*DI:LI:AI*/


import org.combinators.cogen.paradigm.{AddTypeParameter, Apply, GetTypeArguments, ParametricPolymorphism as PP}
import org.combinators.cogen.{Command, Understands}
import org.combinators.ep.language.inbetween.any.AnyParadigm2
import org.combinators.ep.language.inbetween.any


trait ParametricPolymorphism2(val base: AnyParadigm2.WithAST[ParametricPolymorphismAST]) extends PP {
  import base.ast.factory
  import base.ast.polymorphismFactory
  import base.ast.polymorphism.*
  import base.ast.any
  override type TypeParameterContext = TypeParameter
  
  override val methodBodyCapabilities: MethodBodyCapabilities = new MethodBodyCapabilities {
    override implicit val canAddTypeParameterInMethod: Understands[any.Method, AddTypeParameter[any.Name, TypeParameterContext]] =
      new Understands[any.Method, AddTypeParameter[any.Name, TypeParameterContext]] {
        override def perform(context: any.Method, command: AddTypeParameter[any.Name, TypeParameter]): (any.Method, Unit) = {
          val emptyTypeParam = polymorphismFactory.typeParameter(command.name)
          val (tpeParam, _) = Command.runGenerator(command.spec, emptyTypeParam)
          val currentMethod = factory.convert(context)
          val withTypeParameter = currentMethod.copyAsTypeParamMethod(typeParameters = currentMethod.typeParameters :+ tpeParam)
          (withTypeParameter, ())
        }
      }
    override implicit val canGetTypeArgumentsInMethod: Understands[any.Method, GetTypeArguments[any.Type]] =
      new Understands[any.Method, GetTypeArguments[any.Type]] {
        override def perform(context: any.Method, command: GetTypeArguments[any.Type]): (any.Method, Seq[any.Type]) = {
          (context, factory.convert(context).typeParameters.map(_.toTypeArgument))
        }
      }
    override implicit val canApplyTypeInMethod: Understands[any.Method, Apply[any.Type, any.Type, any.Type]] =
      new Understands[any.Method, Apply[any.Type, any.Type, any.Type]] {
        override def perform(context: any.Method, command: Apply[any.Type, any.Type, any.Type]): (any.Method, any.Type) = {
          (context, polymorphismFactory.typeApplication(command.functional, command.arguments))
        }
      }
    override implicit val canApplyMethodToTypeInMethod: Understands[any.Method, Apply[any.Expression, any.Type, any.Expression]] =
      new Understands[any.Method, Apply[any.Expression, any.Type, any.Expression]] {
        override def perform(context: any.Method, command: Apply[any.Expression, any.Type, any.Expression]): (any.Method, any.Expression) = {
          (context, factory.applyExpression(command.functional, command.arguments.map(polymorphismFactory.typeReferenceExpression)))
        }
      }
  }
}

object ParametricPolymorphism2 {
  type WithBase[AST <: ParametricPolymorphismAST, B <: AnyParadigm2.WithAST[AST]] = ParametricPolymorphism2 {val base: B}
  trait WB[AST <: ParametricPolymorphismAST, B <: AnyParadigm2.WithAST[AST]](override val base: B) extends ParametricPolymorphism2 {}
  def apply[AST <: ParametricPolymorphismAST, B <: AnyParadigm2.WithAST[AST]](_base: B): WithBase[AST, _base.type] = new WB[AST, _base.type](_base) with ParametricPolymorphism2(_base) {}
}