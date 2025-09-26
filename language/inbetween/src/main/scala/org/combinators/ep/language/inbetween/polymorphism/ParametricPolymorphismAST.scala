package org.combinators.ep.language.inbetween.polymorphism

import org.combinators.cogen.Command.Generator
import org.combinators.cogen.TypeRep
import org.combinators.ep.language.inbetween.any.AnyAST

trait ParametricPolymorphismAST extends AnyAST {
  object polymorphism {
    object anyOverrides {
      trait FinalTypes extends any.FinalTypes {
        type Method <: anyOverrides.Method
      }

      trait Method extends any.Method {
        def typeParameters: Seq[TypeParameter]

        override def copy(
          name: any.Name = this.name,
          imports: Set[any.Import] = this.imports,
          statements: Seq[any.Statement] = this.statements,
          returnType: Option[any.Type] = this.returnType,
          parameters: Seq[(any.Name, any.Type)] = this.parameters,
          typeLookupMap: TypeRep => Generator[any.Method, any.Type] = this.typeLookupMap
        ): any.Method =
          copyAsTypeParamMethod(
            name = name,
            imports = imports,
            statements = statements,
            returnType = returnType,
            parameters = parameters,
            typeLookupMap = typeLookupMap)

        def copyAsTypeParamMethod(
          name: any.Name = this.name,
          imports: Set[any.Import] = this.imports,
          statements: Seq[any.Statement] = this.statements,
          returnType: Option[any.Type] = this.returnType,
          typeParameters: Seq[TypeParameter] = this.typeParameters,
          parameters: Seq[(any.Name, any.Type)] = this.parameters,
          typeLookupMap: TypeRep => Generator[any.Method, any.Type] = this.typeLookupMap
        ): Method =
          polymorphismFactory.typeParamMethod(name, imports, statements, returnType, typeParameters, parameters, typeLookupMap)
      }
      
      trait Factory extends any.Factory {
        override def method(
          name: any.Name,
          imports: Set[any.Import] = Set.empty,
          statements: Seq[any.Statement] = Seq.empty,
          returnType: Option[any.Type] = Option.empty,
          parameters: Seq[(any.Name, any.Type)] = Seq.empty,
          typeLookupMap: TypeRep => Generator[any.Method, any.Type] = Map.empty
        ): any.Method = polymorphismFactory.typeParamMethod(name, imports, statements, returnType, Seq.empty, parameters, typeLookupMap)
      }
    }
    
    trait FinalTypes extends any.FinalTypes {
      type TypeReferenceExpression <: Expression
      type TypeParameter
      type TypeArgument <: Type
      type TypeApplication <: Type
    }

    trait TypeParameter {
      def getSelfTypeParameter: polymorphismFinalTypes.TypeParameter

      def name: any.Name

      def toTypeArgument: TypeArgument = polymorphismFactory.typeArgument(name)
      def copy(name: any.Name): TypeParameter = polymorphismFactory.typeParameter(name)
    }

    trait TypeArgument extends any.Type {
      def getSelfTypeArgument: polymorphismFinalTypes.TypeArgument
      def name: any.Name
      def copy(name: any.Name): TypeArgument = polymorphismFactory.typeArgument(name)
    }

    trait TypeApplication extends any.Type {
      def getSelfTypeApplication: polymorphismFinalTypes.TypeApplication

      def function: any.Type

      def arguments: Seq[any.Type]

      def copy(
        function: any.Type = this.function,
        arguments: Seq[any.Type] = this.arguments
      ): TypeApplication = polymorphismFactory.typeApplication(function, arguments)

    }

    

    trait TypeReferenceExpression extends any.Expression {
      def getSelfTypeReferenceExpression: polymorphismFinalTypes.TypeReferenceExpression

      def tpe: any.Type

      def copy(
        tpe: any.Type = this.tpe
      ): TypeReferenceExpression = polymorphismFactory.typeReferenceExpression(tpe)
    }

    trait Factory extends any.Factory {
      def typeParameter(name: any.Name): TypeParameter
      def typeArgument(name: any.Name): TypeArgument
      def typeApplication(function: any.Type, arguments: Seq[any.Type]): TypeApplication
      def typeParamMethod(
        name: any.Name,
        imports: Set[any.Import] = Set.empty,
        statements: Seq[any.Statement] = Seq.empty,
        returnType: Option[any.Type] = Option.empty,
        typeParameters: Seq[TypeParameter] = Seq.empty,
        parameters: Seq[(any.Name, any.Type)] = Seq.empty,
        typeLookupMap: TypeRep => Generator[any.Method, any.Type] = Map.empty
      ): anyOverrides.Method
      def typeReferenceExpression(tpe: any.Type): TypeReferenceExpression
      
      implicit def convert(other: TypeReferenceExpression): polymorphismFinalTypes.TypeReferenceExpression = other.getSelfTypeReferenceExpression
      implicit def convert(other: TypeParameter): polymorphismFinalTypes.TypeParameter = other.getSelfTypeParameter
      implicit def convert(other: TypeArgument): polymorphismFinalTypes.TypeArgument = other.getSelfTypeArgument
      implicit def convert(other: TypeApplication): polymorphismFinalTypes.TypeApplication = other.getSelfTypeApplication
    }
  }

  val finalTypes: polymorphism.anyOverrides.FinalTypes
  val polymorphismFinalTypes: polymorphism.FinalTypes
  val factory: polymorphism.anyOverrides.Factory
  val polymorphismFactory: polymorphism.Factory
}
