package org.combinators.ep.language.inbetween.functional    /*DI:LI:AI*/

import org.combinators.cogen.{FileWithPath, TypeRep}
import org.combinators.cogen.Command.Generator
import org.combinators.cogen.paradigm.ffi.FFI
import org.combinators.ep.language.inbetween.any.AnyAST

trait FunctionalAST extends AnyAST {
  object functional {
    object anyOverrides {
      trait FinalTypes extends any.FinalTypes {
        type Project <: anyOverrides.Project
        type CompilationUnit <: anyOverrides.CompilationUnit
        type Method <: anyOverrides.Method
      }

      trait Project extends any.Project {
        def adtTypeLookupMap: TypeRep => Generator[AlgebraicDataType, any.Type] = Map.empty
        def functionTypeLookupMap: TypeRep => Generator[any.Method, any.Type] = Map.empty

        def adtReifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[AlgebraicDataType, any.Expression] = tpe => ???
        def functionReifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[any.Method, any.Expression] = tpe => ???

        def addTypeLookupsForFunctions(lookups: TypeRep => Option[Generator[any.Method, any.Type]]): any.Project =
          copyAsFunctionalProject(functionTypeLookupMap = (tpeRep: TypeRep) => lookups(tpeRep).getOrElse(this.functionTypeLookupMap(tpeRep)))

        def addTypeLookupsForAlgebraicDataTypes(lookups: TypeRep => Option[Generator[AlgebraicDataType, any.Type]]): any.Project =
          copyAsFunctionalProject(adtTypeLookupMap = (tpeRep: TypeRep) => lookups(tpeRep).getOrElse(this.adtTypeLookupMap(tpeRep)))

        def addReifyLookupsForFunctions(lookups: (tpe: TypeRep) => tpe.HostType => Option[Generator[any.Method, any.Expression]]): any.Project =
          copyAsFunctionalProject(functionReifyLookupMap = (tpeRep: TypeRep) => value => lookups(tpeRep)(value).getOrElse(this.functionReifyLookupMap(tpeRep)(value)))

        def addReifyLookupsForAlgebraicDataTypes(lookups: (tpe: TypeRep) => tpe.HostType => Option[Generator[AlgebraicDataType, any.Expression]]): any.Project =
          copyAsFunctionalProject(adtReifyLookupMap = (tpeRep: TypeRep) => value => lookups(tpeRep)(value).getOrElse(this.adtReifyLookupMap(tpeRep)(value)))

        override def copy(
          compilationUnits: Set[any.CompilationUnit],
          customFiles: Seq[FileWithPath],
          enabledFFIs: Set[FFI],
        ): any.Project = copyAsFunctionalProject(compilationUnits, customFiles, enabledFFIs)

        def copyAsFunctionalProject(
          compilationUnits: Set[any.CompilationUnit] = this.compilationUnits,
          customFiles: Seq[FileWithPath] = this.customFiles,
          enabledFFIs: Set[FFI] = this.enabledFFIs,
          adtTypeLookupMap: TypeRep => Generator[AlgebraicDataType, any.Type] = this.adtTypeLookupMap,
          functionTypeLookupMap: TypeRep => Generator[any.Method, any.Type] = this.functionTypeLookupMap,
          adtReifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[AlgebraicDataType, any.Expression] = this.adtReifyLookupMap,
          functionReifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[any.Method, any.Expression] = this.functionReifyLookupMap,
        ): Project = functionalFactory.functionalProject(compilationUnits, customFiles, enabledFFIs, adtTypeLookupMap, functionTypeLookupMap, adtReifyLookupMap, functionReifyLookupMap)
      }

      trait CompilationUnit extends any.CompilationUnit {
        def adts: Seq[AlgebraicDataType] = Seq.empty
        def functions: Seq[any.Method] = Seq.empty

        def adtTypeLookupMap: TypeRep => Generator[AlgebraicDataType, any.Type] = Map.empty
        def functionTypeLookupMap: TypeRep => Generator[any.Method, any.Type] = Map.empty

        def adtReifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[AlgebraicDataType, any.Expression] = tpe => ???
        def functionReifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[any.Method, any.Expression] = tpe => ???

        def resolveImport(tpe: any.Type): Seq[any.Import]
        def resolveImport(exp: any.Expression): Seq[any.Import]

        def addTypeLookupsForFunctions(lookups: TypeRep => Option[Generator[any.Method, any.Type]]): any.CompilationUnit =
          copyAsFunctionalCompilationUnit(functionTypeLookupMap = (tpeRep: TypeRep) => lookups(tpeRep).getOrElse(this.functionTypeLookupMap(tpeRep)))

        def addTypeLookupsForAlgebraicDataTypes(lookups: TypeRep => Option[Generator[AlgebraicDataType, any.Type]]): any.CompilationUnit =
          copyAsFunctionalCompilationUnit(adtTypeLookupMap = (tpeRep: TypeRep) => lookups(tpeRep).getOrElse(this.adtTypeLookupMap(tpeRep)))

        def addReifyLookupsForFunctions(lookups: (tpe: TypeRep) => tpe.HostType => Option[Generator[any.Method, any.Expression]]): any.CompilationUnit =
          copyAsFunctionalCompilationUnit(functionReifyLookupMap = (tpeRep: TypeRep) => value => lookups(tpeRep)(value).getOrElse(this.functionReifyLookupMap(tpeRep)(value)))

        def addReifyLookupsForAlgebraicDataTypes(lookups: (tpe: TypeRep) => tpe.HostType => Option[Generator[AlgebraicDataType, any.Expression]]): any.CompilationUnit =
          copyAsFunctionalCompilationUnit(adtReifyLookupMap = (tpeRep: TypeRep) => value => lookups(tpeRep)(value).getOrElse(this.adtReifyLookupMap(tpeRep)(value)))

        override def initializeInProject(project: any.Project): any.CompilationUnit = {
          import factory.convert
          val withLookups = copyAsFunctionalCompilationUnit(
            adtTypeLookupMap = project.adtTypeLookupMap,
            functionTypeLookupMap = project.functionTypeLookupMap
          )
          withLookups.copyAsFunctionalCompilationUnit(
            tests = withLookups.tests.map(_.initializeInCompilationUnit(withLookups))
          )
        }

        override def copy(
          name: Seq[any.Name] = this.name,
          imports: Seq[any.Import] = this.imports,
          tests: Seq[any.TestSuite] = this.tests,
        ): any.CompilationUnit = copyAsFunctionalCompilationUnit(name, imports, tests = tests)

        def copyAsFunctionalCompilationUnit(
          name: Seq[any.Name] = this.name,
          imports: Seq[any.Import] = this.imports,
          adtTypeLookupMap: TypeRep => Generator[AlgebraicDataType, any.Type] = this.adtTypeLookupMap,
          functionTypeLookupMap: TypeRep => Generator[any.Method, any.Type] = this.functionTypeLookupMap,
          adtReifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[AlgebraicDataType, any.Expression] = this.adtReifyLookupMap,
          functionReifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[any.Method, any.Expression] = this.functionReifyLookupMap,
          adts: Seq[AlgebraicDataType] = this.adts,
          functions: Seq[any.Method] = this.functions,
          tests: Seq[any.TestSuite] = this.tests,
        ): CompilationUnit = functionalFactory.funCompilationUnit(name, imports, adtTypeLookupMap, functionTypeLookupMap, adtReifyLookupMap, functionReifyLookupMap, adts, functions, tests)
      }

      trait Method extends any.Method {
        def resolveImport(exp: any.Expression): Seq[any.Import]

        def findMethod(name: Seq[any.Name]): any.Expression
        def findType(name: Seq[any.Name]): any.Type
      }

      trait Factory extends any.Factory {
        override def project(compilationUnits: Set[any.CompilationUnit], customFiles: Seq[FileWithPath], enabledFFIs: Set[FFI]): any.Project =
          functionalFactory.functionalProject(
            compilationUnits = compilationUnits,
            customFiles = customFiles,
            enabledFFIs = enabledFFIs, 
            adtTypeLookupMap = Map.empty,
            functionTypeLookupMap = Map.empty,
            adtReifyLookupMap = tpe => ???,
            functionReifyLookupMap = tpe => ???,
          )
        override def compilationUnit(name: Seq[any.Name], imports: Seq[any.Import], tests: Seq[any.TestSuite]): any.CompilationUnit =
          functionalFactory.funCompilationUnit(name, imports, adtTypeLookupMap = Map.empty, functionTypeLookupMap = Map.empty, adtReifyLookupMap = tpe => ???, functionReifyLookupMap = tpe => ???, adts = Seq.empty, functions = Seq.empty, tests = tests)
      }
    }
    
    trait FinalTypes {
      type AlgebraicDataType <: functional.AlgebraicDataType
      type TypeConstructor <: functional.TypeConstructor
      type TypeInstantiationExpression <: functional.TypeInstantiationExpression
      type ADTReferenceType <: functional.ADTReferenceType
    }

    trait AlgebraicDataType {
      def getSelfAlgebraicDataType: functionalFinalTypes.AlgebraicDataType

      def name: any.Name
      def imports: Seq[any.Import] = Seq.empty
      def typeConstructors: Seq[TypeConstructor] = Seq.empty

      def typeLookupMap: TypeRep => Generator[AlgebraicDataType, any.Type] = Map.empty

      def reifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[AlgebraicDataType, any.Expression] = tpe => ???

      def toTargetLanguageType(tpe: TypeRep): Generator[AlgebraicDataType, any.Type] = typeLookupMap(tpe)

      def addTypeLookupsForAlgebraicDataTypes(lookups: TypeRep => Option[Generator[AlgebraicDataType, any.Type]]): AlgebraicDataType = {
        copy(typeLookupMap = (tpeRep: TypeRep) => lookups(tpeRep).getOrElse(this.typeLookupMap(tpeRep)))
      }

      def addReifyLookupsForAlgebraicDataTypes(lookups: (tpe: TypeRep) => tpe.HostType => Option[Generator[AlgebraicDataType, any.Expression]]): AlgebraicDataType =
        copy(reifyLookupMap = (tpeRep: TypeRep) => value => lookups(tpeRep)(value).getOrElse(this.reifyLookupMap(tpeRep)(value)))

      def findType(name: Seq[any.Name]): any.Type

      def resolveImport(tpe: any.Type): Seq[any.Import]
      def resolveImport(exp: any.Expression): Seq[any.Import]
      def getFreshName(basedOn: any.Name): any.Name

      def copy(
        name: any.Name = this.name,
        imports: Seq[any.Import] = this.imports,
        typeConstructors: Seq[TypeConstructor] = this.typeConstructors,
        typeLookupMap: TypeRep => Generator[AlgebraicDataType, any.Type] = this.typeLookupMap,
        reifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[AlgebraicDataType, any.Expression] = this.reifyLookupMap,
      ): AlgebraicDataType = functionalFactory.adt(
        name = name,
        imports = imports,
        typeConstructors = typeConstructors,
        typeLookupMap = typeLookupMap,
        reifyLookupMap = reifyLookupMap,
      )
    }

    trait TypeConstructor {
      def getSelfTypeConstructor: functionalFinalTypes.TypeConstructor

      def name: any.Name
      def parameters: Seq[(any.Name, any.Type)]

      def copy(
        name: any.Name = this.name,
        parameters: Seq[(any.Name, any.Type)] = this.parameters
      ): TypeConstructor = functionalFactory.typeConstructor(name, parameters)
    }

    trait TypeInstantiationExpression extends any.Expression {
      def getSelfTypeInstantiationExpression: functionalFinalTypes.TypeInstantiationExpression

      def tpe: any.Type
      def constructorName: Seq[any.Name]
      def constructorArguments: Seq[any.Expression]

      def copy(
        tpe: any.Type = this.tpe,
        constructorName: Seq[any.Name] = this.constructorName,
        constructorArguments: Seq[any.Expression] = this.constructorArguments
      ): TypeInstantiationExpression = functionalFactory.typeInstantiationExpression(tpe, constructorName, constructorArguments)
    }

    trait ADTReferenceType extends any.Type {
      def getSelfADTReferenceType: functionalFinalTypes.ADTReferenceType

      def qualifiedTypeName: Seq[any.Name]

      def copy(
        qualifiedTypeName: Seq[any.Name] = this.qualifiedTypeName
      ): ADTReferenceType = functionalFactory.adtReferenceType(qualifiedTypeName *)
    }

    trait Factory {
      def functionalProject(
        compilationUnits: Set[any.CompilationUnit] = Set.empty,
        customFiles: Seq[FileWithPath] = Seq.empty,
        enabledFFIs: Set[FFI] = Set.empty,
        adtTypeLookupMap: TypeRep => Generator[functional.AlgebraicDataType, any.Type] = Map.empty,
        functionTypeLookupMap: TypeRep => Generator[any.Method, any.Type] = Map.empty,
        adtReifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[AlgebraicDataType, any.Expression] = tpe => ???,
        functionReifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[any.Method, any.Expression] = tpe => ???,
      ): anyOverrides.Project
      
      def funCompilationUnit(
        name: Seq[any.Name],
        imports: Seq[any.Import],
        adtTypeLookupMap: TypeRep => Generator[functional.AlgebraicDataType, any.Type] = Map.empty,
        functionTypeLookupMap: TypeRep => Generator[any.Method, any.Type] = Map.empty,
        adtReifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[AlgebraicDataType, any.Expression] = tpe => ???,
        functionReifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[any.Method, any.Expression] = tpe => ???,
        adts: Seq[functional.AlgebraicDataType] = Seq.empty,
        functions: Seq[any.Method] = Seq.empty,
        tests: Seq[any.TestSuite] = Seq.empty,
      ): anyOverrides.CompilationUnit

      def adt(
        name: any.Name,
        imports: Seq[any.Import] = Seq.empty,
        typeConstructors: Seq[TypeConstructor] = Seq.empty,
        typeLookupMap: TypeRep => Generator[functional.AlgebraicDataType, any.Type] = Map.empty,
        reifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[AlgebraicDataType, any.Expression] = tpe => ???,
      ): AlgebraicDataType

      def typeConstructor(
        name: any.Name,
        parameters: Seq[(any.Name, any.Type)] = Seq.empty
      ): TypeConstructor

      def typeInstantiationExpression(
        tpe: any.Type,
        constructorName: Seq[any.Name],
        constructorArguments: Seq[any.Expression] = Seq.empty,
      ): TypeInstantiationExpression

      def adtReferenceType(qualifiedTypeName: any.Name*): ADTReferenceType

      implicit def convert(other: AlgebraicDataType): functionalFinalTypes.AlgebraicDataType = other.getSelfAlgebraicDataType
      implicit def convert(other: TypeConstructor): functionalFinalTypes.TypeConstructor = other.getSelfTypeConstructor
      implicit def convert(other: TypeInstantiationExpression): functionalFinalTypes.TypeInstantiationExpression = other.getSelfTypeInstantiationExpression
      implicit def convert(other: ADTReferenceType): functionalFinalTypes.ADTReferenceType = other.getSelfADTReferenceType
    }
  }

  val finalTypes: functional.anyOverrides.FinalTypes
  val functionalFinalTypes: functional.FinalTypes
  val factory: functional.anyOverrides.Factory
  val functionalFactory: functional.Factory
}
