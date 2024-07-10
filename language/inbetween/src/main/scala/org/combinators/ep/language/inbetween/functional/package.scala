package org.combinators.ep.language.inbetween   /*DI:LI:AI*/

import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.generator.Command
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.language.inbetween.any.{CompilationUnit, Project, TestSuite}


package object functional {
  trait FinalTypes extends any.FinalTypes {
    type AlgebraicDataType
    type TypeConstructor
    type TypeInstantiationExpression
  }

  trait Project[FT <: FinalTypes] extends any.Project[FT] with Factory[FT] {
    def adtTypeLookupMap: TypeRep => Generator[AlgebraicDataType[FT], any.Type[FT]] = Map.empty
    def functionTypeLookupMap: TypeRep => Generator[any.Method[FT], any.Type[FT]] = Map.empty

    override def addTypeLookupsForFunctions(lookups: TypeRep => Option[Generator[any.Method[FT], any.Type[FT]]]): any.Project[FT] =
      copyAsFunctionalProject(functionTypeLookupMap = (tpeRep: TypeRep) => lookups(tpeRep).getOrElse(this.functionTypeLookupMap(tpeRep)))

    def addTypeLookupsForAlgebraicDataTypes(lookups: TypeRep => Option[Generator[AlgebraicDataType[FT], any.Type[FT]]]): any.Project[FT] =
      copyAsFunctionalProject(adtTypeLookupMap = (tpeRep: TypeRep) => lookups(tpeRep).getOrElse(this.adtTypeLookupMap(tpeRep)))

    override def copy(
      compilationUnits: Set[any.CompilationUnit[FT]]
    ): Project[FT] = copyAsFunctionalProject(compilationUnits)

    def copyAsFunctionalProject(
      compilationUnits: Set[any.CompilationUnit[FT]] = this.compilationUnits,
      adtTypeLookupMap: TypeRep => Generator[AlgebraicDataType[FT], any.Type[FT]] = this.adtTypeLookupMap,
      functionTypeLookupMap: TypeRep => Generator[any.Method[FT], any.Type[FT]] = this.functionTypeLookupMap,
    ): Project[FT] = functionalProject(compilationUnits, adtTypeLookupMap, functionTypeLookupMap)
  }

  trait CompilationUnit[FT <: FinalTypes] extends any.CompilationUnit[FT] with Factory[FT] {
    def adts: Seq[AlgebraicDataType[FT]] = Seq.empty
    def functions: Seq[any.Method[FT]] = Seq.empty

    def adtTypeLookupMap: TypeRep => Generator[AlgebraicDataType[FT], any.Type[FT]] = Map.empty
    def functionTypeLookupMap: TypeRep => Generator[any.Method[FT], any.Type[FT]] = Map.empty

    def resolveImport(tpe: any.Type[FT]): Seq[any.Import[FT]]
    def resolveImport(exp: any.Expression[FT]): Seq[any.Import[FT]]

    def addTypeLookupsForFunctions(lookups: TypeRep => Option[Generator[any.Method[FT], any.Type[FT]]]): any.CompilationUnit[FT] =
      copyAsFunctionalCompilationUnit(functionTypeLookupMap = (tpeRep: TypeRep) => lookups(tpeRep).getOrElse(this.functionTypeLookupMap(tpeRep)))

    def addTypeLookupsForAlgebraicDataTypes(lookups: TypeRep => Option[Generator[AlgebraicDataType[FT], any.Type[FT]]]): any.CompilationUnit[FT] =
      copyAsFunctionalCompilationUnit(adtTypeLookupMap = (tpeRep: TypeRep) => lookups(tpeRep).getOrElse(this.adtTypeLookupMap(tpeRep)))

    override def initializeInProject(project: any.Project[FT]): any.CompilationUnit[FT] = {
      val withLookups = copyAsFunctionalCompilationUnit(
        adtTypeLookupMap = project.adtTypeLookupMap,
        functionTypeLookupMap = project.functionTypeLookupMap
      )
      withLookups.copyAsFunctionalCompilationUnit(
        tests = withLookups.tests.map(_.initializeInCompilationUnit(withLookups))
      )
    }

    override def copy(
      name: Seq[any.Name[FT]] = this.name,
      imports: Seq[any.Import[FT]] = this.imports,
      tests: Seq[any.TestSuite[FT]] = this.tests,
    ): CompilationUnit[FT] = copyAsFunctionalCompilationUnit(name, imports, tests = tests)

    def copyAsFunctionalCompilationUnit(
      name: Seq[any.Name[FT]] = this.name,
      imports: Seq[any.Import[FT]] = this.imports,
      adtTypeLookupMap: TypeRep => Generator[AlgebraicDataType[FT], any.Type[FT]] = this.adtTypeLookupMap,
      functionTypeLookupMap: TypeRep => Generator[any.Method[FT], any.Type[FT]] = this.functionTypeLookupMap,
      adts: Seq[AlgebraicDataType[FT]] = this.adts,
      functions: Seq[any.Method[FT]] = this.functions,
      tests: Seq[any.TestSuite[FT]] = this.tests,
    ): CompilationUnit[FT] = compilationUnit(name, imports, adtTypeLookupMap, functionTypeLookupMap, adts, functions, tests)
  }

  trait AlgebraicDataType[FT <: FinalTypes] extends Factory[FT] {
    def getSelfAlgebraicDataType: finalTypes.AlgebraicDataType

    def name: any.Name[FT]
    def imports: Seq[any.Import[FT]] = Seq.empty
    def typeConstructors: Seq[TypeConstructor[FT]] = Seq.empty

    def typeLookupMap: TypeRep => Generator[AlgebraicDataType[FT], any.Type[FT]] = Map.empty

    def toTargetLanguageType(tpe: TypeRep): Generator[AlgebraicDataType[FT], any.Type[FT]] = typeLookupMap(tpe)

    def addTypeLookupsForAlgebraicDataTypes(lookups: TypeRep => Option[Generator[AlgebraicDataType[FT], any.Type[FT]]]): AlgebraicDataType[FT] = {
      copy(typeLookupMap = (tpeRep: TypeRep) => lookups(tpeRep).getOrElse(this.typeLookupMap(tpeRep)))
    }

    def findType(name: Seq[any.Name[FT]]): any.Type[FT]

    def resolveImport(tpe: any.Type[FT]): Seq[any.Import[FT]]
    def resolveImport(exp: any.Expression[FT]): Seq[any.Import[FT]]
    def getFreshName(basedOn: any.Name[FT]): any.Name[FT]

    def copy(
      name: any.Name[FT] = this.name,
      imports: Seq[any.Import[FT]] = this.imports,
      typeConstructors: Seq[TypeConstructor[FT]] = this.typeConstructors,
      typeLookupMap: TypeRep => Generator[AlgebraicDataType[FT], any.Type[FT]] = this.typeLookupMap,
    ): AlgebraicDataType[FT] = adt(
      name = name,
      imports = imports,
      typeConstructors = typeConstructors,
      typeLookupMap = typeLookupMap,
    )
  }

  trait TypeConstructor[FT <: FinalTypes] extends Factory[FT]{
    def getSelfTypeConstructor: finalTypes.TypeConstructor
    
    def name: any.Name[FT]
    def parameters: Seq[(any.Name[FT], any.Type[FT])]

    def copy(
      name: any.Name[FT] = this.name,
      parameters: Seq[(any.Name[FT], any.Type[FT])] = this.parameters
    ): TypeConstructor[FT] = typeConstructor(name, parameters)
  }

  trait TypeInstantiationExpression[FT <: FinalTypes] extends any.Expression[FT] with Factory[FT] {
    def getSelfTypeInstantiationExpression: finalTypes.TypeInstantiationExpression

    def tpe: any.Type[FT]
    def constructorName: Seq[any.Name[FT]]
    def constructorArguments: Seq[any.Expression[FT]]

    def copy(
      tpe: any.Type[FT] = this.tpe,
      constructorName: Seq[any.Name[FT]] = this.constructorName,
      constructorArguments: Seq[any.Expression[FT]] = this.constructorArguments
    ): TypeInstantiationExpression[FT] = typeInstantiationExpression(tpe, constructorName, constructorArguments)
  }

  trait Method[FT <: FinalTypes] extends any.Method[FT] {
    def resolveImport(exp: any.Expression[FT]): Seq[any.Import[FT]]

    def findMethod(name: Seq[any.Name[FT]]): any.Expression[FT]
    def findType(name: Seq[any.Name[FT]]): any.Type[FT]
  }

  trait Factory[FT <: FinalTypes] extends any.Factory[FT] {

    override def project(compilationUnits: Set[any.CompilationUnit[FT]]): Project[FT] =
      functionalProject(compilationUnits = compilationUnits, adtTypeLookupMap=Map.empty, functionTypeLookupMap=Map.empty)


    def functionalProject(
      compilationUnits: Set[any.CompilationUnit[FT]] = Set.empty,
      adtTypeLookupMap: TypeRep => Generator[AlgebraicDataType[FT], any.Type[FT]] = Map.empty,
      functionTypeLookupMap: TypeRep => Generator[any.Method[FT], any.Type[FT]] = Map.empty,
    ): any.Project[FT]


    override def compilationlUnit(name: Seq[any.Name[FT]], imports: Seq[any.Import[FT]], tests: Seq[any.TestSuite[FT]]): CompilationUnit[FT] =
      compilationUnit(name, imports, adtTypeLookupMap=Map.empty, functionTypeLookupMap=Map.empty, adts=Seq.empty, functions=Seq.empty, tests = tests)

    def compilationUnit(
      name: Seq[any.Name[FT]],
      imports: Seq[any.Import[FT]],
      adtTypeLookupMap: TypeRep => Generator[AlgebraicDataType[FT], any.Type[FT]] = Map.empty,
      functionTypeLookupMap: TypeRep => Generator[any.Method[FT], any.Type[FT]] = Map.empty,
      adts: Seq[AlgebraicDataType[FT]] = Seq.empty,
      functions: Seq[any.Method[FT]] = Seq.empty,
      tests: Seq[any.TestSuite[FT]] = Seq.empty,
    ): CompilationUnit[FT]

    override def method(
      name: any.Name[FT],
      imports: Set[any.Import[FT]] = Set.empty,
      statements: Seq[any.Statement[FT]] = Seq.empty,
      returnType: Option[any.Type[FT]] = Option.empty,
      parameters: Seq[(any.Name[FT], any.Type[FT])] = Seq.empty,
      typeLookupMap: TypeRep => Generator[any.Method[FT], any.Type[FT]] = Map.empty
    ): any.Method[FT] = fnMethod(name, imports, statements, returnType, parameters, typeLookupMap)

    def fnMethod(
      name: any.Name[FT],
      imports: Set[any.Import[FT]] = Set.empty,
      statements: Seq[any.Statement[FT]] = Seq.empty,
      returnType: Option[any.Type[FT]] = Option.empty,
      parameters: Seq[(any.Name[FT], any.Type[FT])] = Seq.empty,
      typeLookupMap: TypeRep => Generator[any.Method[FT], any.Type[FT]] = Map.empty
    ): Method[FT]

    def adt(
      name: any.Name[FT],
      imports: Seq[any.Import[FT]] = Seq.empty,
      typeConstructors: Seq[TypeConstructor[FT]] = Seq.empty,
      typeLookupMap: TypeRep => Generator[AlgebraicDataType[FT], any.Type[FT]] = Map.empty,
    ): AlgebraicDataType[FT]

    def typeConstructor(
      name: any.Name[FT],
      parameters: Seq[(any.Name[FT], any.Type[FT])] = Seq.empty
    ): TypeConstructor[FT]

    def typeInstantiationExpression(
      tpe: any.Type[FT],
      constructorName: Seq[any.Name[FT]],
      constructorArguments: Seq[any.Expression[FT]] = Seq.empty,
    ): TypeInstantiationExpression[FT]

    override implicit def convert(other: any.Project[FT]): Project[FT]
    override implicit def convert(other: any.CompilationUnit[FT]): CompilationUnit[FT]
    override implicit def convert(other: any.Method[FT]): Method[FT]

    implicit def convert(other: AlgebraicDataType[FT]): AlgebraicDataType[FT]
    implicit def convert(other: TypeConstructor[FT]): TypeConstructor[FT]
    implicit def convert(other: TypeInstantiationExpression[FT]): TypeInstantiationExpression[FT]
  }
}