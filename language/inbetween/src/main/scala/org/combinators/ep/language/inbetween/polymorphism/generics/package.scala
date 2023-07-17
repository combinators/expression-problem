package org.combinators.ep.language.inbetween.polymorphism

import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.language.inbetween.{any, oo, polymorphism => pp}
import org.combinators.ep.generator.Command.Generator
package object generics {
  trait FinalTypes extends pp.FinalTypes with oo.FinalTypes {

  }

  trait TypeParameter[FT <: FinalTypes] extends pp.TypeParameter[FT] with Factory[FT] {
    def upperBounds: Seq[any.Type[FT]]
    def lowerBounds: Seq[any.Type[FT]]

    override def copy(name: any.Name[FT] = this.name): TypeParameter[FT] = copyAsTypeParameterWithBounds(name)
    def copyAsTypeParameterWithBounds(
                                       name: any.Name[FT] = this.name,
                                       upperBounds: Seq[any.Type[FT]] = this.upperBounds,
                                       lowerBounds: Seq[any.Type[FT]] = this.lowerBounds): TypeParameter[FT] =
      typeParameterWithBounds(name, upperBounds, lowerBounds)
  }

  trait Class[FT <: FinalTypes] extends oo.Class[FT] with Factory[FT] {
    def typeParameters: Seq[pp.TypeParameter[FT]]

    override def copy(
              name: any.Name[FT] = this.name,
              imports: Seq[any.Import[FT]] = this.imports,
              parents: Seq[any.Type[FT]] = this.parents,
              implemented: Seq[any.Type[FT]] = this.implemented,
              fields: Seq[oo.Field[FT]] = this.fields,
              methods: Seq[any.Method[FT]] = this.methods,
              constructors: Seq[oo.Constructor[FT]] = this.constructors,
              methodTypeLookupMap: TypeRep => Generator[any.Method[FT], any.Type[FT]] = this.methodTypeLookupMap,
              constructorTypeLookupMap: TypeRep => Generator[oo.Constructor[FT], any.Type[FT]] = this.constructorTypeLookupMap,
              typeLookupMap: TypeRep => Generator[oo.Class[FT], any.Type[FT]] = this.typeLookupMap,
              isAbstract: Boolean = this.isAbstract,
              isInterface: Boolean = this.isInterface,
              isStatic: Boolean = this.isStatic,
            ): oo.Class[FT] = copyAsGenericClass(
      name = name,
      imports = imports,
      parents = parents,
      implemented = implemented,
      fields = fields,
      methods = methods,
      constructors = constructors,
      methodTypeLookupMap = methodTypeLookupMap,
      constructorTypeLookupMap = constructorTypeLookupMap,
      typeLookupMap = typeLookupMap,
      isAbstract = isAbstract,
      isInterface = isInterface,
      isStatic = isStatic,
    )

    def copyAsGenericClass(
              name: any.Name[FT] = this.name,
              imports: Seq[any.Import[FT]] = this.imports,
              typeParameters: Seq[pp.TypeParameter[FT]] = this.typeParameters,
              parents: Seq[any.Type[FT]] = this.parents,
              implemented: Seq[any.Type[FT]] = this.implemented,
              fields: Seq[oo.Field[FT]] = this.fields,
              methods: Seq[any.Method[FT]] = this.methods,
              constructors: Seq[oo.Constructor[FT]] = this.constructors,
              methodTypeLookupMap: TypeRep => Generator[any.Method[FT], any.Type[FT]] = this.methodTypeLookupMap,
              constructorTypeLookupMap: TypeRep => Generator[oo.Constructor[FT], any.Type[FT]] = this.constructorTypeLookupMap,
              typeLookupMap: TypeRep => Generator[oo.Class[FT], any.Type[FT]] = this.typeLookupMap,
              isAbstract: Boolean = this.isAbstract,
              isInterface: Boolean = this.isInterface,
              isStatic: Boolean = this.isStatic,
            ): oo.Class[FT] = genericClass(
      name = name,
      imports = imports,
      typeParameters = typeParameters,
      parents = parents,
      implemented = implemented,
      fields = fields,
      methods = methods,
      constructors = constructors,
      methodTypeLookupMap = methodTypeLookupMap,
      constructorTypeLookupMap = constructorTypeLookupMap,
      typeLookupMap = typeLookupMap,
      isAbstract = isAbstract,
      isInterface = isInterface,
      isStatic = isStatic,
    )

  }

  trait Method[FT <: FinalTypes] extends pp.Method[FT] with oo.Method[FT] with Factory[FT] {


    override def copy(
      name: any.Name[FT] = this.name,
      imports: Set[any.Import[FT]] = this.imports,
      statements: Seq[any.Statement[FT]] = this.statements,
      returnType: Option[any.Type[FT]] = this.returnType,
      parameters: Seq[(any.Name[FT], any.Type[FT])] = this.parameters,
      typeLookupMap: TypeRep => Generator[any.Method[FT], any.Type[FT]] = this.typeLookupMap
    ): any.Method[FT] =
      copyAsGenericMethod(
        name = name,
        imports = imports,
        statements = statements,
        returnType = returnType,
        parameters = parameters,
        typeLookupMap = typeLookupMap)

    override def copyAsTypeParamMethod(name: any.Name[FT] = this.name,
                                       imports: Set[any.Import[FT]] = this.imports,
                                       statements: Seq[any.Statement[FT]] = this.statements,
                                       returnType: Option[any.Type[FT]] = this.returnType,
                                       typeParameters: Seq[pp.TypeParameter[FT]] = this.typeParameters,
                                       parameters: Seq[(any.Name[FT], any.Type[FT])] = this.parameters,
                                       typeLookupMap: TypeRep => Generator[any.Method[FT], any.Type[FT]] = this.typeLookupMap): pp.Method[FT] =
      copyAsGenericMethod(
        name = name,
        imports = imports,
        statements = statements,
        returnType = returnType,
        typeParameters = typeParameters,
        parameters = parameters,
        typeLookupMap = typeLookupMap)

    override def copyAsClsMethod(name: any.Name[FT] = this.name,
                                 imports: Set[any.Import[FT]] = this.imports,
                                 statements: Seq[any.Statement[FT]] = this.statements,
                                 returnType: Option[any.Type[FT]] = this.returnType,
                                 parameters: Seq[(any.Name[FT], any.Type[FT])] = this.parameters,
                                 typeLookupMap: TypeRep => Generator[any.Method[FT], any.Type[FT]] = this.typeLookupMap,
                                 isAbstract: Boolean = this.isAbstract,
                                 isStatic: Boolean = this.isStatic,
                                 isPublic: Boolean = this.isPublic,
                                 isOverride: Boolean = this.isOverride): oo.Method[FT] =
      copyAsGenericMethod(name = name,
        imports = imports,
        statements = statements,
        returnType = returnType,
        parameters = parameters,
        typeLookupMap = typeLookupMap,
        isAbstract = isAbstract,
        isStatic = isStatic,
        isPublic = isPublic,
        isOverride = isOverride)

    def copyAsGenericMethod(
                             name: any.Name[FT] = this.name,
                             imports: Set[any.Import[FT]] = this.imports,
                             statements: Seq[any.Statement[FT]] = this.statements,
                             returnType: Option[any.Type[FT]] = this.returnType,
                             typeParameters: Seq[pp.TypeParameter[FT]] = this.typeParameters,
                             parameters: Seq[(any.Name[FT], any.Type[FT])] = this.parameters,
                             typeLookupMap: TypeRep => Generator[any.Method[FT], any.Type[FT]] = this.typeLookupMap,
                             isAbstract: Boolean = this.isAbstract,
                             isStatic: Boolean = this.isStatic,
                             isPublic: Boolean = this.isPublic,
                             isOverride: Boolean = this.isOverride,
                           ) =
      genericMethod(name, imports, statements, returnType, typeParameters, parameters, typeLookupMap, isAbstract, isStatic, isPublic, isOverride)
  }

  trait Factory[FT <: FinalTypes] extends pp.Factory[FT] with oo.Factory[FT] {

    override def cls(
             name: any.Name[FT],
             imports: Seq[any.Import[FT]] = Seq.empty,
             parents: Seq[any.Type[FT]] = Seq.empty,
             implemented: Seq[any.Type[FT]] = Seq.empty,
             fields: Seq[oo.Field[FT]] = Seq.empty,
             methods: Seq[any.Method[FT]] = Seq.empty,
             constructors: Seq[oo.Constructor[FT]] = Seq.empty,
             methodTypeLookupMap: TypeRep => Generator[any.Method[FT], any.Type[FT]] = Map.empty,
             constructorTypeLookupMap: TypeRep => Generator[oo.Constructor[FT], any.Type[FT]] = Map.empty,
             typeLookupMap: TypeRep => Generator[oo.Class[FT], any.Type[FT]] = Map.empty,
             isAbstract: Boolean = false,
             isInterface: Boolean = false,
             isStatic: Boolean = false,
           ): oo.Class[FT] =
      genericClass(
        name,
        imports,
        Seq.empty,
        parents,
        implemented,
        fields,
        methods,
        constructors,
        methodTypeLookupMap,
        constructorTypeLookupMap,
        typeLookupMap,
        isAbstract,
        isInterface,
        isStatic,
      )

    def genericClass(
             name: any.Name[FT],
             imports: Seq[any.Import[FT]] = Seq.empty,
             typeParameters: Seq[pp.TypeParameter[FT]] = Seq.empty,
             parents: Seq[any.Type[FT]] = Seq.empty,
             implemented: Seq[any.Type[FT]] = Seq.empty,
             fields: Seq[oo.Field[FT]] = Seq.empty,
             methods: Seq[any.Method[FT]] = Seq.empty,
             constructors: Seq[oo.Constructor[FT]] = Seq.empty,
             methodTypeLookupMap: TypeRep => Generator[any.Method[FT], any.Type[FT]] = Map.empty,
             constructorTypeLookupMap: TypeRep => Generator[oo.Constructor[FT], any.Type[FT]] = Map.empty,
             typeLookupMap: TypeRep => Generator[oo.Class[FT], any.Type[FT]] = Map.empty,
             isAbstract: Boolean = false,
             isInterface: Boolean = false,
             isStatic: Boolean = false,
           ): oo.Class[FT]

    override def clsMethod(
                   name: any.Name[FT],
                   imports: Set[any.Import[FT]] = Set.empty,
                   statements: Seq[any.Statement[FT]] = Seq.empty,
                   returnType: Option[any.Type[FT]] = Option.empty,
                   parameters: Seq[(any.Name[FT], any.Type[FT])] = Seq.empty,
                   typeLookupMap: TypeRep => Generator[any.Method[FT], any.Type[FT]] = Map.empty,
                   isAbstract: Boolean = false,
                   isStatic: Boolean = false,
                   isPublic: Boolean = false,
                   isOverride: Boolean = false
                 ): Method[FT] =
      genericMethod(name, imports, statements, returnType, Seq.empty, parameters, typeLookupMap, isAbstract, isStatic, isPublic, isOverride)

    override def typeParamMethod(name: any.Name[FT],
                                 imports: Set[any.Import[FT]] = Set.empty,
                                 statements: Seq[any.Statement[FT]] = Seq.empty,
                                 returnType: Option[any.Type[FT]] = Option.empty,
                                 typeParameters: Seq[pp.TypeParameter[FT]] = Seq.empty,
                                 parameters: Seq[(any.Name[FT], any.Type[FT])] = Seq.empty,
                                 typeLookupMap: TypeRep => Generator[any.Method[FT], any.Type[FT]] = Map.empty): pp.Method[FT] =
      genericMethod(name, imports, statements, returnType, typeParameters, parameters, typeLookupMap)

    def genericMethod(
                       name: any.Name[FT],
                       imports: Set[any.Import[FT]] = Set.empty,
                       statements: Seq[any.Statement[FT]] = Seq.empty,
                       returnType: Option[any.Type[FT]] = Option.empty,
                       typeParameters: Seq[pp.TypeParameter[FT]] = Seq.empty,
                       parameters: Seq[(any.Name[FT], any.Type[FT])] = Seq.empty,
                       typeLookupMap: TypeRep => Generator[any.Method[FT], any.Type[FT]] = Map.empty,
                       isAbstract: Boolean = false,
                       isStatic: Boolean = false,
                       isPublic: Boolean = false,
                       isOverride: Boolean = false,
                     ): Method[FT]

    override def typeParameter(name: any.Name[FT]): pp.TypeParameter[FT] = typeParameterWithBounds(name)
    def typeParameterWithBounds(name: any.Name[FT], upperBounds: Seq[any.Type[FT]] = Seq.empty, lowerBounds: Seq[any.Type[FT]] = Seq.empty): TypeParameter[FT]

    def convert(other: oo.Class[FT]): Class[FT]
    def convert(other: any.Method[FT]): Method[FT]
    def convert(other: pp.TypeParameter[FT]): TypeParameter[FT]
  }

}
