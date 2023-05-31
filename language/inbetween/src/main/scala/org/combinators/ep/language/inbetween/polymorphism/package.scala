package org.combinators.ep.language.inbetween

import org.combinators.ep.domain.abstractions.TypeRep

package object polymorphism {
  trait FinalTypes extends any.FinalTypes {
    type TypeReferenceExpression <: Expression
    type TypeParameter
    type TypeArgument <: Type
    type TypeApplication <: Type
  }

  trait TypeParameter[FT <: FinalTypes] extends Factory[FT] {
    def getSelfTypeParameter: finalTypes.TypeParameter

    def name: any.Name[FT]

    def toTypeArgument: TypeArgument[FT] = typeArgument(name)
    def copy(name: any.Name[FT]): TypeParameter[FT] = typeParameter(name)
  }

  trait TypeArgument[FT <: FinalTypes] extends any.Type[FT] with Factory[FT] {
    def getSelfTypeArgument: finalTypes.TypeArgument
    def name: any.Name[FT]
    def copy(name: any.Name[FT]): TypeArgument[FT] = typeArgument(name)
  }

  trait TypeApplication[FT <: FinalTypes] extends any.Type[FT] with Factory[FT] {
    def getSelfTypeApplication: finalTypes.TypeApplication

    def function: any.Type[FT]

    def arguments: Seq[any.Type[FT]]

    def copy(
              function: any.Type[FT] = this.function,
              arguments: Seq[any.Type[FT]] = this.arguments
            ): TypeApplication[FT] = typeApplication(function, arguments)

  }

  trait Method[FT <: FinalTypes] extends any.Method[FT] with Factory[FT] {
    def typeParameters: Seq[TypeParameter[FT]]

    override def copy(
                       name: any.Name[FT] = this.name,
                       imports: Set[any.Import[FT]] = this.imports,
                       statements: Seq[any.Statement[FT]] = this.statements,
                       returnType: Option[any.Type[FT]] = this.returnType,
                       parameters: Seq[(any.Name[FT], any.Type[FT])] = this.parameters,
                       typeLookupMap: Map[TypeRep, any.Type[FT]] = this.typeLookupMap
                     ): any.Method[FT] =
      typeParamMethod(name, imports, statements, returnType, this.typeParameters, parameters, typeLookupMap)

    def copyAsTypeParamMethod(
                       name: any.Name[FT] = this.name,
                       imports: Set[any.Import[FT]] = this.imports,
                       statements: Seq[any.Statement[FT]] = this.statements,
                       returnType: Option[any.Type[FT]] = this.returnType,
                       typeParameters: Seq[TypeParameter[FT]] = this.typeParameters,
                       parameters: Seq[(any.Name[FT], any.Type[FT])] = this.parameters,
                       typeLookupMap: Map[TypeRep, any.Type[FT]] = this.typeLookupMap
                     ): Method[FT] =
      typeParamMethod(name, imports, statements, returnType, typeParameters, parameters, typeLookupMap)
  }

  trait TypeReferenceExpression[FT <: FinalTypes] extends any.Expression[FT] with Factory[FT] {
    def getSelfTypeReferenceExpression: finalTypes.TypeReferenceExpression

    def tpe: any.Type[FT]

    def copy(
              tpe: any.Type[FT] = this.tpe
            ): TypeReferenceExpression[FT] = typeReferenceExpression(tpe)
  }

  trait Factory[FT <: FinalTypes] extends any.Factory[FT] {
    def typeParameter(name: any.Name[FT]): TypeParameter[FT]
    def typeArgument(name: any.Name[FT]): TypeArgument[FT]

    def typeApplication(function: any.Type[FT], arguments: Seq[any.Type[FT]]): TypeApplication[FT]
    override def method(
                         name: any.Name[FT],
                         imports: Set[any.Import[FT]] = Set.empty,
                         statements: Seq[any.Statement[FT]] = Seq.empty,
                         returnType: Option[any.Type[FT]] = Option.empty,
                         parameters: Seq[(any.Name[FT], any.Type[FT])] = Seq.empty,
                         typeLookupMap: Map[TypeRep, any.Type[FT]] = Map.empty
                       ): any.Method[FT] = typeParamMethod(name, imports, statements, returnType, Seq.empty, parameters, typeLookupMap)

    def typeParamMethod(
                   name: any.Name[FT],
                   imports: Set[any.Import[FT]] = Set.empty,
                   statements: Seq[any.Statement[FT]] = Seq.empty,
                   returnType: Option[any.Type[FT]] = Option.empty,
                   typeParameters: Seq[TypeParameter[FT]] = Seq.empty,
                   parameters: Seq[(any.Name[FT], any.Type[FT])] = Seq.empty,
                   typeLookupMap: Map[TypeRep, any.Type[FT]] = Map.empty
                 ): Method[FT]
    def typeReferenceExpression(tpe: any.Type[FT]): TypeReferenceExpression[FT]
    implicit def convert(other: TypeReferenceExpression[FT]): TypeReferenceExpression[FT]
    implicit def convert(other: TypeParameter[FT]): TypeParameter[FT]
    implicit def convert(other: TypeArgument[FT]): TypeArgument[FT]
    implicit def convert(other: TypeApplication[FT]): TypeApplication[FT]
    implicit def convert(other: any.Method[FT]): Method[FT]
  }
}
