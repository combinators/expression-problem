package org.combinators.ep.language.inbetween.polymorphism.generics

import org.combinators.cogen.Command.Generator
import org.combinators.cogen.TypeRep
import org.combinators.ep.language.inbetween.oo.OOAST
import org.combinators.ep.language.inbetween.polymorphism.ParametricPolymorphismAST

trait GenericsAST extends ParametricPolymorphismAST with OOAST {
  object generics {
    object anyOverrides {
      trait FinalTypes extends oo.anyOverrides.FinalTypes with polymorphism.anyOverrides.FinalTypes {
        type Method <: anyOverrides.Method
      }

      trait Method extends polymorphism.anyOverrides.Method with oo.anyOverrides.Method {
        override def copy(
          name: any.Name = this.name,
          imports: Set[any.Import] = this.imports,
          statements: Seq[any.Statement] = this.statements,
          returnType: Option[any.Type] = this.returnType,
          parameters: Seq[(any.Name, any.Type)] = this.parameters,
          typeLookupMap: TypeRep => Generator[any.Method, any.Type] = this.typeLookupMap
        ): any.Method =
          copyAsGenericMethod(
            name = name,
            imports = imports,
            statements = statements,
            returnType = returnType,
            parameters = parameters,
            typeLookupMap = typeLookupMap)

        override def copyAsTypeParamMethod(
          name: any.Name = this.name,
          imports: Set[any.Import] = this.imports,
          statements: Seq[any.Statement] = this.statements,
          returnType: Option[any.Type] = this.returnType,
          typeParameters: Seq[polymorphism.TypeParameter] = this.typeParameters,
          parameters: Seq[(any.Name, any.Type)] = this.parameters,
          typeLookupMap: TypeRep => Generator[any.Method, any.Type] = this.typeLookupMap
        ): polymorphism.anyOverrides.Method =
          copyAsGenericMethod(
            name = name,
            imports = imports,
            statements = statements,
            returnType = returnType,
            typeParameters = typeParameters,
            parameters = parameters,
            typeLookupMap = typeLookupMap)

        override def copyAsClsMethod(
          name: any.Name = this.name,
          imports: Set[any.Import] = this.imports,
          statements: Seq[any.Statement] = this.statements,
          returnType: Option[any.Type] = this.returnType,
          parameters: Seq[(any.Name, any.Type)] = this.parameters,
          typeLookupMap: TypeRep => Generator[any.Method, any.Type] = this.typeLookupMap,
          isAbstract: Boolean = this.isAbstract,
          isStatic: Boolean = this.isStatic,
          isPublic: Boolean = this.isPublic,
          isOverride: Boolean = this.isOverride
        ): oo.anyOverrides.Method =
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
          name: any.Name = this.name,
          imports: Set[any.Import] = this.imports,
          statements: Seq[any.Statement] = this.statements,
          returnType: Option[any.Type] = this.returnType,
          typeParameters: Seq[polymorphism.TypeParameter] = this.typeParameters,
          parameters: Seq[(any.Name, any.Type)] = this.parameters,
          typeLookupMap: TypeRep => Generator[any.Method, any.Type] = this.typeLookupMap,
          isAbstract: Boolean = this.isAbstract,
          isStatic: Boolean = this.isStatic,
          isPublic: Boolean = this.isPublic,
          isOverride: Boolean = this.isOverride,
        ): Method =
          genericsFactory.genericMethod(name, imports, statements, returnType, typeParameters, parameters, typeLookupMap, isAbstract, isStatic, isPublic, isOverride)
      }
      
      trait Factory extends oo.anyOverrides.Factory with polymorphism.anyOverrides.Factory {
      }
    }
    object ooOverrides {
      trait FinalTypes extends oo.FinalTypes {
        type Class <: ooOverrides.Class
      }

      trait Class extends oo.Class {
        def typeParameters: Seq[polymorphism.TypeParameter]

        override def copy(
          name: any.Name = this.name,
          imports: Seq[any.Import] = this.imports,
          parents: Seq[any.Type] = this.parents,
          implemented: Seq[any.Type] = this.implemented,
          fields: Seq[oo.Field] = this.fields,
          methods: Seq[any.Method] = this.methods,
          constructors: Seq[oo.Constructor] = this.constructors,
          methodTypeLookupMap: TypeRep => Generator[any.Method, any.Type] = this.methodTypeLookupMap,
          constructorTypeLookupMap: TypeRep => Generator[oo.Constructor, any.Type] = this.constructorTypeLookupMap,
          typeLookupMap: TypeRep => Generator[oo.Class, any.Type] = this.typeLookupMap,
          isAbstract: Boolean = this.isAbstract,
          isInterface: Boolean = this.isInterface,
          isStatic: Boolean = this.isStatic,
        ): oo.Class = copyAsGenericClass(
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
          name: any.Name = this.name,
          imports: Seq[any.Import] = this.imports,
          typeParameters: Seq[polymorphism.TypeParameter] = this.typeParameters,
          parents: Seq[any.Type] = this.parents,
          implemented: Seq[any.Type] = this.implemented,
          fields: Seq[oo.Field] = this.fields,
          methods: Seq[any.Method] = this.methods,
          constructors: Seq[oo.Constructor] = this.constructors,
          methodTypeLookupMap: TypeRep => Generator[any.Method, any.Type] = this.methodTypeLookupMap,
          constructorTypeLookupMap: TypeRep => Generator[oo.Constructor, any.Type] = this.constructorTypeLookupMap,
          typeLookupMap: TypeRep => Generator[oo.Class, any.Type] = this.typeLookupMap,
          isAbstract: Boolean = this.isAbstract,
          isInterface: Boolean = this.isInterface,
          isStatic: Boolean = this.isStatic,
        ): oo.Class = genericsFactory.genericClass(
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
      
      trait Factory extends oo.Factory {
        override def cls(
          name: any.Name,
          imports: Seq[any.Import] = Seq.empty,
          parents: Seq[any.Type] = Seq.empty,
          implemented: Seq[any.Type] = Seq.empty,
          fields: Seq[oo.Field] = Seq.empty,
          methods: Seq[any.Method] = Seq.empty,
          constructors: Seq[oo.Constructor] = Seq.empty,
          methodTypeLookupMap: TypeRep => Generator[any.Method, any.Type] = Map.empty,
          constructorTypeLookupMap: TypeRep => Generator[oo.Constructor, any.Type] = Map.empty,
          typeLookupMap: TypeRep => Generator[oo.Class, any.Type] = Map.empty,
          isAbstract: Boolean = false,
          isInterface: Boolean = false,
          isStatic: Boolean = false,
        ): oo.Class =
          genericsFactory.genericClass(
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
        override def clsMethod(
            name: any.Name,
            imports: Set[any.Import] = Set.empty,
            statements: Seq[any.Statement] = Seq.empty,
            returnType: Option[any.Type] = Option.empty,
            parameters: Seq[(any.Name, any.Type)] = Seq.empty,
            typeLookupMap: TypeRep => Generator[any.Method, any.Type] = Map.empty,
            isAbstract: Boolean = false,
            isStatic: Boolean = false,
            isPublic: Boolean = false,
            isOverride: Boolean = false
          ): oo.anyOverrides.Method =
            genericsFactory.genericMethod(name, imports, statements, returnType, Seq.empty, parameters, typeLookupMap, isAbstract, isStatic, isPublic, isOverride)
          
      }

    }
    
    object polymorphismOverrides {
      trait FinalTypes extends polymorphism.FinalTypes {
        type TypeParameter <: polymorphismOverrides.TypeParameter
      }

      trait TypeParameter extends polymorphism.TypeParameter {
        def upperBounds: Seq[any.Type]
        def lowerBounds: Seq[any.Type]

        override def copy(name: any.Name = this.name): polymorphism.TypeParameter = copyAsTypeParameterWithBounds(name)
        def copyAsTypeParameterWithBounds(
          name: any.Name = this.name,
          upperBounds: Seq[any.Type] = this.upperBounds,
          lowerBounds: Seq[any.Type] = this.lowerBounds
        ): TypeParameter =
          genericsFactory.typeParameterWithBounds(name, upperBounds, lowerBounds)
      }
      
      trait Factory extends polymorphism.Factory {
        override def typeParamMethod(
          name: any.Name,
          imports: Set[any.Import] = Set.empty,
          statements: Seq[any.Statement] = Seq.empty,
          returnType: Option[any.Type] = Option.empty,
          typeParameters: Seq[polymorphism.TypeParameter] = Seq.empty,
          parameters: Seq[(any.Name, any.Type)] = Seq.empty,
          typeLookupMap: TypeRep => Generator[any.Method, any.Type] = Map.empty
        ): polymorphism.anyOverrides.Method =
          genericsFactory.genericMethod(name, imports, statements, returnType, typeParameters, parameters, typeLookupMap)
        override def typeParameter(name: any.Name): polymorphism.TypeParameter = genericsFactory.typeParameterWithBounds(name)
      }
    }

    trait Factory {
      def genericClass(
        name: any.Name,
        imports: Seq[any.Import] = Seq.empty,
        typeParameters: Seq[polymorphism.TypeParameter] = Seq.empty,
        parents: Seq[any.Type] = Seq.empty,
        implemented: Seq[any.Type] = Seq.empty,
        fields: Seq[oo.Field] = Seq.empty,
        methods: Seq[any.Method] = Seq.empty,
        constructors: Seq[oo.Constructor] = Seq.empty,
        methodTypeLookupMap: TypeRep => Generator[any.Method, any.Type] = Map.empty,
        constructorTypeLookupMap: TypeRep => Generator[oo.Constructor, any.Type] = Map.empty,
        typeLookupMap: TypeRep => Generator[oo.Class, any.Type] = Map.empty,
        isAbstract: Boolean = false,
        isInterface: Boolean = false,
        isStatic: Boolean = false,
      ): ooOverrides.Class
      
      def genericMethod(
        name: any.Name,
        imports: Set[any.Import] = Set.empty,
        statements: Seq[any.Statement] = Seq.empty,
        returnType: Option[any.Type] = Option.empty,
        typeParameters: Seq[polymorphism.TypeParameter] = Seq.empty,
        parameters: Seq[(any.Name, any.Type)] = Seq.empty,
        typeLookupMap: TypeRep => Generator[any.Method, any.Type] = Map.empty,
        isAbstract: Boolean = false,
        isStatic: Boolean = false,
        isPublic: Boolean = false,
        isOverride: Boolean = false,
      ): anyOverrides.Method

      def typeParameterWithBounds(name: any.Name, upperBounds: Seq[any.Type] = Seq.empty, lowerBounds: Seq[any.Type] = Seq.empty): polymorphismOverrides.TypeParameter
    }
  }

  val finalTypes: generics.anyOverrides.FinalTypes
  val ooFinalTypes: generics.ooOverrides.FinalTypes
  val polymorphismFinalTypes: generics.polymorphismOverrides.FinalTypes
  
  val factory: generics.anyOverrides.Factory
  val ooFactory: generics.ooOverrides.Factory
  val polymorphismFactory: generics.polymorphismOverrides.Factory
  val genericsFactory: generics.Factory
}
