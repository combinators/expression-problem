package org.combinators.ep.language.inbetween.oo    /*DI:LI:AI*/

import org.combinators.cogen.{FileWithPath, TypeRep}
import org.combinators.cogen.Command.Generator
import org.combinators.ep.language.inbetween.any.AnyAST

trait OOAST extends AnyAST {

  object oo {
    object anyOverrides {
      trait FinalTypes extends any.FinalTypes {
        type Method <: anyOverrides.Method
        type Expression <: anyOverrides.Expression
        type TestSuite <: anyOverrides.TestSuite
        type CompilationUnit <: anyOverrides.CompilationUnit
        type Project <: anyOverrides.Project
      }

      trait Project extends any.Project {
        def methodTypeLookupMap: TypeRep => Generator[any.Method, any.Type] = Map.empty
        def constructorTypeLookupMap: TypeRep => Generator[Constructor, any.Type] = Map.empty
        def classTypeLookupMap: TypeRep => Generator[Class, any.Type] = Map.empty

        def methodReifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[any.Method, any.Expression] = tpe => ???
        def constructorReifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[Constructor, any.Expression] = tpe => ???
        def classReifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[Class, any.Expression] = tpe => ???

        override def addTypeLookupsForMethods(lookups: TypeRep => Option[Generator[any.Method, any.Type]]): any.Project =
          copyAsProjectWithTypeLookups(methodTypeLookupMap = (tpeRep: TypeRep) => lookups(tpeRep).getOrElse(this.methodTypeLookupMap(tpeRep)))

        def addTypeLookupsForConstructors(lookups: TypeRep => Option[Generator[oo.Constructor, any.Type]]): any.Project =
          copyAsProjectWithTypeLookups(constructorTypeLookupMap = (tpeRep: TypeRep) => lookups(tpeRep).getOrElse(this.constructorTypeLookupMap(tpeRep)))

        def addTypeLookupsForClasses(lookups: TypeRep => Option[Generator[oo.Class, any.Type]]): any.Project =
          copyAsProjectWithTypeLookups(classTypeLookupMap = (tpeRep: TypeRep) => lookups(tpeRep).getOrElse(this.classTypeLookupMap(tpeRep)))

        override def addReifyLookupsForMethods(lookups: (tpe: TypeRep) => tpe.HostType => Option[Generator[any.Method, any.Expression]]): any.Project =
          copyAsProjectWithTypeLookups(methodReifyLookupMap = (tpeRep: TypeRep) => value => lookups(tpeRep)(value).getOrElse(this.methodReifyLookupMap(tpeRep)(value)))

        def addReifyLookupsForConstructors(lookups: (tpe: TypeRep) => tpe.HostType => Option[Generator[Constructor, any.Expression]]): any.Project =
          copyAsProjectWithTypeLookups(constructorReifyLookupMap = (tpeRep: TypeRep) => value => lookups(tpeRep)(value).getOrElse(this.constructorReifyLookupMap(tpeRep)(value)))

        def addReifyLookupsForClasses(lookups: (tpe: TypeRep) => tpe.HostType => Option[Generator[Class, any.Expression]]): any.Project =
          copyAsProjectWithTypeLookups(classReifyLookupMap = (tpeRep: TypeRep) => value => lookups(tpeRep)(value).getOrElse(this.classReifyLookupMap(tpeRep)(value)))

        override def copy(
          compilationUnits: Set[any.CompilationUnit],
          customFiles: Seq[FileWithPath]
        ): any.Project = copyAsProjectWithTypeLookups(compilationUnits, customFiles)

        def copyAsProjectWithTypeLookups(
          compilationUnits: Set[any.CompilationUnit] = this.compilationUnits,
          customFiles: Seq[FileWithPath] = this.customFiles,
          methodTypeLookupMap: TypeRep => Generator[any.Method, any.Type] = this.methodTypeLookupMap,
          constructorTypeLookupMap: TypeRep => Generator[Constructor, any.Type] = this.constructorTypeLookupMap,
          classTypeLookupMap: TypeRep => Generator[Class, any.Type] = this.classTypeLookupMap,
          methodReifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[any.Method, any.Expression] = this.methodReifyLookupMap,
          constructorReifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[Constructor, any.Expression] = this.constructorReifyLookupMap,
          classReifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[Class, any.Expression] = this.classReifyLookupMap,
        ): Project = ooFactory.ooProject(
          compilationUnits,
          customFiles,
          methodTypeLookupMap,
          constructorTypeLookupMap,
          classTypeLookupMap,
          methodReifyLookupMap,
          constructorReifyLookupMap,
          classReifyLookupMap,
        )
      }

      trait CompilationUnit extends any.CompilationUnit {
        def classes: Seq[Class] = Seq.empty
        def methodTypeLookupMap: TypeRep => Generator[any.Method, any.Type] = Map.empty
        def constructorTypeLookupMap: TypeRep => Generator[Constructor, any.Type] = Map.empty
        def classTypeLookupMap: TypeRep => Generator[Class, any.Type] = Map.empty
        def methodReifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[any.Method, any.Expression] = tpe => ???
        def constructorReifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[Constructor, any.Expression] = tpe => ???
        def classReifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[Class, any.Expression] = tpe => ???

        def addTypeLookupsForMethods(lookups: TypeRep => Option[Generator[any.Method, any.Type]]): any.CompilationUnit =
          copyAsCompilationUnitWithClasses(methodTypeLookupMap = (tpeRep: TypeRep) => lookups(tpeRep).getOrElse(this.methodTypeLookupMap(tpeRep)))

        def addTypeLookupsForConstructors(lookups: TypeRep => Option[Generator[oo.Constructor, any.Type]]): any.CompilationUnit =
          copyAsCompilationUnitWithClasses(constructorTypeLookupMap = (tpeRep: TypeRep) => lookups(tpeRep).getOrElse(this.constructorTypeLookupMap(tpeRep)))

        def addTypeLookupsForClasses(lookups: TypeRep => Option[Generator[oo.Class, any.Type]]): any.CompilationUnit =
          copyAsCompilationUnitWithClasses(classTypeLookupMap = (tpeRep: TypeRep) => lookups(tpeRep).getOrElse(this.classTypeLookupMap(tpeRep)))

        def addReifyLookupsForMethods(lookups: (tpe: TypeRep) => tpe.HostType => Option[Generator[any.Method, any.Expression]]): any.CompilationUnit =
          copyAsCompilationUnitWithClasses(methodReifyLookupMap = (tpeRep: TypeRep) => value => lookups(tpeRep)(value).getOrElse(this.methodReifyLookupMap(tpeRep)(value)))

        def addReifyLookupsForConstructors(lookups: (tpe: TypeRep) => tpe.HostType => Option[Generator[Constructor, any.Expression]]): any.CompilationUnit =
          copyAsCompilationUnitWithClasses(constructorReifyLookupMap = (tpeRep: TypeRep) => value => lookups(tpeRep)(value).getOrElse(this.constructorReifyLookupMap(tpeRep)(value)))

        def addReifyLookupsForClasses(lookups: (tpe: TypeRep) => tpe.HostType => Option[Generator[Class, any.Expression]]): any.CompilationUnit =
          copyAsCompilationUnitWithClasses(classReifyLookupMap = (tpeRep: TypeRep) => value => lookups(tpeRep)(value).getOrElse(this.classReifyLookupMap(tpeRep)(value)))

        override def initializeInProject(project: any.Project): any.CompilationUnit = {
          import factory.convert
          val withLookups = copyAsCompilationUnitWithClasses(
            classTypeLookupMap = project.classTypeLookupMap,
            constructorTypeLookupMap = project.constructorTypeLookupMap,
            methodTypeLookupMap = project.methodTypeLookupMap,
            methodReifyLookupMap = project.methodReifyLookupMap,
            constructorReifyLookupMap = project.constructorReifyLookupMap,
            classReifyLookupMap = project.classReifyLookupMap,
          )
          withLookups.copyAsCompilationUnitWithClasses(
            tests = withLookups.tests.map(_.initializeInCompilationUnit(withLookups))
          )
        }

        override def copy(
          name: Seq[any.Name] = this.name,
          imports: Seq[any.Import] = this.imports,
          tests: Seq[any.TestSuite] = this.tests,
        ): any.CompilationUnit = copyAsCompilationUnitWithClasses(name, imports, tests = tests)

        def copyAsCompilationUnitWithClasses(
          name: Seq[any.Name] = this.name,
          imports: Seq[any.Import] = this.imports,
          methodTypeLookupMap: TypeRep => Generator[any.Method, any.Type] = this.methodTypeLookupMap,
          constructorTypeLookupMap: TypeRep => Generator[Constructor, any.Type] = this.constructorTypeLookupMap,
          classTypeLookupMap: TypeRep => Generator[Class, any.Type] = this.classTypeLookupMap,
          methodReifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[any.Method, any.Expression] = this.methodReifyLookupMap,
          constructorReifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[Constructor, any.Expression] = this.constructorReifyLookupMap,
          classReifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[Class, any.Expression] = this.classReifyLookupMap,
          classes: Seq[Class] = this.classes,
          tests: Seq[any.TestSuite] = this.tests,
        ): CompilationUnit = ooFactory.ooCompilationUnit(name, imports, methodTypeLookupMap, constructorTypeLookupMap, classTypeLookupMap, methodReifyLookupMap, constructorReifyLookupMap, classReifyLookupMap, classes, tests)
      }

      trait TestSuite extends any.TestSuite {
        def underlyingClass: Class
        def testMarkers: Seq[Boolean]

        override def name: any.Name = underlyingClass.name
        override def tests: Seq[any.Method] = underlyingClass.methods.zip(testMarkers).filter { case (m, isTest) => isTest }.map(_._1)
        override def methodTypeLookupMap: TypeRep => Generator[any.Method, any.Type] = underlyingClass.methodTypeLookupMap
        override def methodReifyLookupMap: (tpe:TypeRep) => tpe.HostType => Generator[any.Method, any.Expression] = underlyingClass.methodReifyLookupMap

        override def initializeInCompilationUnit(compilationUnit: any.CompilationUnit): any.TestSuite = {
          import factory.convert
          copyAsClassBasedTestSuite(
            underlyingClass = underlyingClass.copy(
              constructorTypeLookupMap = compilationUnit.constructorTypeLookupMap,
              methodTypeLookupMap = compilationUnit.methodTypeLookupMap,
              typeLookupMap = compilationUnit.classTypeLookupMap,
              methodReifyLookupMap = compilationUnit.methodReifyLookupMap,
              reifyLookupMap = compilationUnit.classReifyLookupMap,
            ),
            testMarkers
          )
        }

        override def copy(
          name: any.Name = this.name,
          tests: Seq[any.Method] = this.tests,
          methodTypeLookupMap: TypeRep => Generator[any.Method, any.Type] = this.methodTypeLookupMap,
          methodReifyLookupMap: (tpe:TypeRep) => tpe.HostType => Generator[any.Method, any.Expression] = this.methodReifyLookupMap,
        ): any.TestSuite = {
          val helpers = underlyingClass.methods.zip(this.testMarkers).filter { case (m, isTest) => !isTest }.map(_._1)
          val testMarkers = Seq.fill(helpers.size)(false) ++ Seq.fill(tests.size)(true)

          copyAsClassBasedTestSuite(
            underlyingClass = this.underlyingClass.copy(
              name = name,
              methods = helpers ++ tests,
              methodTypeLookupMap = methodTypeLookupMap,
              methodReifyLookupMap = methodReifyLookupMap,
            ),
            testMarkers
          )
        }

        def copyAsClassBasedTestSuite(
          underlyingClass: Class = this.underlyingClass,
          testMarkers: Seq[Boolean] = this.testMarkers
        ): any.TestSuite = ooFactory.classBasedTestSuite(underlyingClass, testMarkers)

      }

      trait Method extends any.Method {
        def isAbstract: Boolean = false
        def isStatic: Boolean = false
        def isPublic: Boolean = false
        def isOverride: Boolean = false

        def findClass(qualifiedName: any.Name*): any.Type

        def addTestExpressions(exprs: Seq[any.Expression]): any.Method

        override def copy(
          name: any.Name = this.name,
          imports: Set[any.Import] = this.imports,
          statements: Seq[any.Statement] = this.statements,
          returnType: Option[any.Type] = this.returnType,
          parameters: Seq[(any.Name, any.Type)] = this.parameters,
          typeLookupMap: TypeRep => Generator[any.Method, any.Type] = this.typeLookupMap,
          reifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[any.Method, any.Expression] = this.reifyLookupMap
        ): any.Method = ooFactory.clsMethod(name, imports, statements, returnType, parameters, typeLookupMap, reifyLookupMap, isAbstract, isStatic, isPublic, isOverride)

        def copyAsClsMethod(
          name: any.Name = this.name,
          imports: Set[any.Import] = this.imports,
          statements: Seq[any.Statement] = this.statements,
          returnType: Option[any.Type] = this.returnType,
          parameters: Seq[(any.Name, any.Type)] = this.parameters,
          typeLookupMap: TypeRep => Generator[any.Method, any.Type] = this.typeLookupMap,
          reifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[any.Method, any.Expression] = this.reifyLookupMap,
          isAbstract: Boolean = this.isAbstract,
          isStatic: Boolean = this.isStatic,
          isPublic: Boolean = this.isPublic,
          isOverride: Boolean = this.isOverride,
        ): Method = ooFactory.clsMethod(name, imports, statements, returnType, parameters, typeLookupMap, reifyLookupMap, isAbstract, isStatic, isPublic, isOverride)
      }

      trait Expression extends any.Expression {

      }

      trait Factory extends any.Factory {
        import ooFactory.*

        override def project(compilationUnits: Set[any.CompilationUnit], customFiles: Seq[FileWithPath]): any.Project =
          ooProject(compilationUnits = compilationUnits, customFiles = customFiles, methodTypeLookupMap = Map.empty, constructorTypeLookupMap = Map.empty, classTypeLookupMap = Map.empty)

        override def compilationUnit(name: Seq[any.Name], imports: Seq[any.Import], tests: Seq[any.TestSuite]): any.CompilationUnit =
          ooCompilationUnit(name, imports, methodTypeLookupMap = Map.empty, constructorTypeLookupMap = Map.empty, classTypeLookupMap = Map.empty, classes = Seq.empty, tests = tests)

        override def method(
          name: any.Name,
          imports: Set[any.Import] = Set.empty,
          statements: Seq[any.Statement] = Seq.empty,
          returnType: Option[any.Type] = Option.empty,
          parameters: Seq[(any.Name, any.Type)] = Seq.empty,
          typeLookupMap: TypeRep => Generator[any.Method, any.Type] = Map.empty
        ): any.Method = clsMethod(name, imports, statements, returnType, parameters, typeLookupMap)

        override def testSuite(name: any.Name, tests: Seq[any.Method], methodTypeLookupMap: TypeRep => Generator[any.Method, any.Type] = Map.empty): any.TestSuite =
          classBasedTestSuite(cls(name = name, methods = tests, methodTypeLookupMap = methodTypeLookupMap), Seq.fill(tests.size)(true))
      }

    }

    trait FinalTypes {
      type Class <: oo.Class
      type Constructor <: oo.Constructor
      type Field <: oo.Field
      type MemberAccessExpression <: oo.MemberAccessExpression
      type SelfReferenceExpression <: oo.SelfReferenceExpression
      type ObjectInstantiationExpression <: oo.ObjectInstantiationExpression
      type CastExpression <: oo.CastExpression
      type InstanceOfExpression <: oo.InstanceOfExpression
      type SuperReferenceExpression <: oo.SuperReferenceExpression
      type ClassReferenceType <: oo.ClassReferenceType
    }

    trait Class  {
      def getSelfClass: ooFinalTypes.Class

      def name: any.Name
      def imports: Seq[any.Import] = Seq.empty
      def parents: Seq[any.Type] = Seq.empty
      def implemented: Seq[any.Type] = Seq.empty
      def fields: Seq[Field] = Seq.empty
      def methods: Seq[any.Method] = Seq.empty
      def constructors: Seq[Constructor] = Seq.empty
      def methodTypeLookupMap: TypeRep => Generator[any.Method, any.Type] = Map.empty
      def constructorTypeLookupMap: TypeRep => Generator[Constructor, any.Type] = Map.empty
      def typeLookupMap: TypeRep => Generator[Class, any.Type] = Map.empty
      def methodReifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[any.Method, any.Expression] = tpe => ???
      def constructorReifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[Constructor, any.Expression] = tpe => ???
      def reifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[Class, any.Expression] = tpe => ???
      def isAbstract: Boolean = false
      def isInterface: Boolean = false
      def isStatic: Boolean = false

      def toTargetLanguageType(tpe: TypeRep): Generator[Class, any.Type] = typeLookupMap(tpe)

      def addTypeLookupsForMethods(lookups: TypeRep => Option[Generator[any.Method, any.Type]]): oo.Class =
        copy(methodTypeLookupMap = (tpeRep: TypeRep) => lookups(tpeRep).getOrElse(this.methodTypeLookupMap(tpeRep)))

      def addTypeLookupsForConstructors(lookups: TypeRep => Option[Generator[oo.Constructor, any.Type]]): oo.Class =
        copy(constructorTypeLookupMap = (tpeRep: TypeRep) => lookups(tpeRep).getOrElse(this.constructorTypeLookupMap(tpeRep)))

      def addTypeLookupsForClasses(lookups: TypeRep => Option[Generator[Class, any.Type]]): oo.Class = {
        copy(typeLookupMap = (tpeRep: TypeRep) => lookups(tpeRep).getOrElse(this.typeLookupMap(tpeRep)))
      }

      def addReifyLookupsForMethods(lookups: (tpe: TypeRep) => tpe.HostType => Option[Generator[any.Method, any.Expression]]): Class =
        copy(methodReifyLookupMap = (tpeRep: TypeRep) => value => lookups(tpeRep)(value).getOrElse(this.methodReifyLookupMap(tpeRep)(value)))

      def addReifyLookupsForConstructors(lookups: (tpe: TypeRep) => tpe.HostType => Option[Generator[Constructor, any.Expression]]): Class =
        copy(constructorReifyLookupMap = (tpeRep: TypeRep) => value => lookups(tpeRep)(value).getOrElse(this.constructorReifyLookupMap(tpeRep)(value)))

      def addReifyLookupsForClasses(lookups: (tpe: TypeRep) => tpe.HostType => Option[Generator[Class, any.Expression]]): Class =
        copy(reifyLookupMap = (tpeRep: TypeRep) => value => lookups(tpeRep)(value).getOrElse(this.reifyLookupMap(tpeRep)(value)))

      def addParent(parent: any.Type): Class =
        copy(parents = (this.parents :+ parent).distinct)

      def addImplemented(impl: any.Type): Class =
        copy(implemented = (this.implemented :+ impl).distinct)

      def addField(field: Field): Class =
        copy(fields = this.fields :+ field) // TODO: make distinct by name

      def getField(name: any.Name): any.Expression =
        ooFactory.memberAccessExpression(ooFactory.selfReferenceExpression, name)

      def addMethod(method: any.Method): Class =
        copy(methods = this.methods :+ method)

      def addConstructor(constructor: Constructor): Class =
        copy(constructors = this.constructors :+ constructor)

      def resolveImport(tpe: any.Type): Seq[any.Import]
      def findClass(qualifiedName: any.Name*): any.Type
      def getFreshName(basedOn: any.Name): any.Name

      def copy(
        name: any.Name = this.name,
        imports: Seq[any.Import] = this.imports,
        parents: Seq[any.Type] = this.parents,
        implemented: Seq[any.Type] = this.implemented,
        fields: Seq[Field] = this.fields,
        methods: Seq[any.Method] = this.methods,
        constructors: Seq[Constructor] = this.constructors,
        methodTypeLookupMap: TypeRep => Generator[any.Method, any.Type] = this.methodTypeLookupMap,
        constructorTypeLookupMap: TypeRep => Generator[Constructor, any.Type] = this.constructorTypeLookupMap,
        typeLookupMap: TypeRep => Generator[Class, any.Type] = this.typeLookupMap,
        methodReifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[any.Method, any.Expression] = this.methodReifyLookupMap,
        constructorReifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[Constructor, any.Expression] = this.constructorReifyLookupMap,
        reifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[Class, any.Expression] = this.reifyLookupMap,
        isAbstract: Boolean = this.isAbstract,
        isInterface: Boolean = this.isInterface,
        isStatic: Boolean = this.isStatic,
      ): Class = ooFactory.cls(
        name = name,
        imports = imports,
        parents = parents,
        implemented = implemented,
        methods = methods,
        fields = fields,
        constructors = constructors,
        methodTypeLookupMap = methodTypeLookupMap,
        constructorTypeLookupMap = constructorTypeLookupMap,
        typeLookupMap = typeLookupMap,
        methodReifyLookupMap = methodReifyLookupMap,
        constructorReifyLookupMap = constructorReifyLookupMap,
        reifyLookupMap = reifyLookupMap,
        isAbstract = isAbstract,
        isInterface = isInterface,
        isStatic = isStatic,
      )
    }

    trait MemberAccessExpression extends anyOverrides.Expression {
      def getSelfMemberAccessExpression: ooFinalTypes.MemberAccessExpression

      def owner: any.Expression
      def field: any.Name

      def copy(
        owner: any.Expression = this.owner,
        field: any.Name = this.field,
      ): MemberAccessExpression = ooFactory.memberAccessExpression(owner = owner, field = field)
    }

    trait SelfReferenceExpression extends anyOverrides.Expression {
      def getSelfSelfReferenceExpression: ooFinalTypes.SelfReferenceExpression
    }

    trait SuperReferenceExpression extends anyOverrides.Expression {
      def getSelfSuperReferenceExpression: ooFinalTypes.SuperReferenceExpression

      def parentType: any.Type

      def copy(
        parentType: any.Type = this.parentType,
      ): SuperReferenceExpression = ooFactory.superReferenceExpression(parentType)
    }

    trait ObjectInstantiationExpression extends anyOverrides.Expression {
      def getSelfObjectInstantiationExpression: ooFinalTypes.ObjectInstantiationExpression

      def tpe: any.Type
      def constructorArguments: Seq[any.Expression]
      def body: Option[Class] = Option.empty

      def copy(
        tpe: any.Type = this.tpe,
        constructorArguments: Seq[any.Expression] = this.constructorArguments,
        body: Option[Class] = this.body
      ): ObjectInstantiationExpression = ooFactory.objectInstantiationExpression(tpe, constructorArguments, body)
    }

    trait CastExpression extends anyOverrides.Expression {
      def getSelfCastExpression: ooFinalTypes.CastExpression

      def tpe: any.Type
      def expression: any.Expression

      def copy(
        tpe: any.Type = this.tpe,
        expression: any.Expression = this.expression
      ): CastExpression = ooFactory.castExpression(tpe, expression)
    }

    trait InstanceOfExpression extends anyOverrides.Expression {
      def getSelfInstanceOfExpression: ooFinalTypes.InstanceOfExpression

      def tpe: any.Type
      def expression: any.Expression

      def copy(
        tpe: any.Type = this.tpe,
        expression: any.Expression = this.expression
      ): InstanceOfExpression = ooFactory.instanceOfExpression(tpe, expression)
    }

    trait Constructor extends anyOverrides.Method {
      def constructorTypeLookupMap: TypeRep => Generator[Constructor, any.Type] = Map.empty
      def constructorReifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[Constructor, any.Expression] = tpe => ???
      def getSelfConstructor: ooFinalTypes.Constructor

      override def isAbstract: Boolean = false
      override def isOverride: Boolean = false

      def constructedType: Option[any.Type] = Option.empty
      def superInitialization: Option[(any.Type, Seq[any.Expression])] = Option.empty
      def fieldInitializers: Seq[(any.Name, any.Expression)] = Seq.empty

      def addConstructorTypeLookups(lookups: TypeRep => Option[Generator[Constructor, any.Type]]): Constructor = {
        copyAsConstructor(constructorTypeLookupMap = (tpeRep: TypeRep) => lookups(tpeRep).getOrElse(this.constructorTypeLookupMap(tpeRep)))
      }
      def toTargetLanguageTypeInConstructor(tpe: TypeRep): Generator[Constructor, any.Type] = constructorTypeLookupMap(tpe)
      def reifyInConstructor[T](tpe: TypeRep.OfHostType[T], value: T): Generator[Constructor, any.Expression] = constructorReifyLookupMap(tpe)(value)

      override def copyAsClsMethod(
        name: any.Name = this.name,
        imports: Set[any.Import] = this.imports,
        statements: Seq[any.Statement] = this.statements,
        returnType: Option[any.Type] = this.returnType,
        parameters: Seq[(any.Name, any.Type)] = this.parameters,
        typeLookupMap: TypeRep => Generator[any.Method, any.Type] = this.typeLookupMap,
        reifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[any.Method, any.Expression] = this.reifyLookupMap,
        isAbstract: Boolean = this.isAbstract,
        isStatic: Boolean = this.isStatic,
        isPublic: Boolean = this.isPublic,
        isOverride: Boolean = this.isOverride,
      ): anyOverrides.Method = copyAsConstructor(this.constructedType, imports, statements, parameters, typeLookupMap, this.constructorTypeLookupMap, reifyLookupMap)

      def copyAsConstructor(
        constructedType: Option[any.Type] = this.constructedType,
        imports: Set[any.Import] = this.imports,
        statements: Seq[any.Statement] = this.statements,
        parameters: Seq[(any.Name, any.Type)] = this.parameters,
        typeLookupMap: TypeRep => Generator[any.Method, any.Type] = this.typeLookupMap,
        constructorTypeLookupMap: TypeRep => Generator[Constructor, any.Type] = this.constructorTypeLookupMap,
        reifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[any.Method, any.Expression] = this.reifyLookupMap,
        constructorReifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[Constructor, any.Expression] = this.constructorReifyLookupMap,
        superInitialization: Option[(any.Type, Seq[any.Expression])] = this.superInitialization,
        fieldInitializers: Seq[(any.Name, any.Expression)] = this.fieldInitializers,
      ): Constructor = ooFactory.constructor(constructedType, imports, statements, parameters, typeLookupMap, constructorTypeLookupMap, reifyLookupMap, constructorReifyLookupMap, superInitialization, fieldInitializers)
    }

    trait Field  {
      def getSelfField: ooFinalTypes.Field

      def name: any.Name
      def tpe: any.Type
      def init: Option[any.Expression] = Option.empty

      def copy(
        name: any.Name = this.name,
        tpe: any.Type = this.tpe,
        init: Option[any.Expression] = this.init,
      ): Field = ooFactory.field(name, tpe, init)
    }

    trait ClassReferenceType extends any.Type {
      def getSelfClassReferenceType: ooFinalTypes.ClassReferenceType

      def qualifiedClassName: Seq[any.Name]

      def copy(
        qualifiedClassName: Seq[any.Name] = this.qualifiedClassName
      ): ClassReferenceType = ooFactory.classReferenceType(qualifiedClassName*)
    }


    trait Factory {
      def ooProject(
        compilationUnits: Set[any.CompilationUnit] = Set.empty,
        customFiles: Seq[FileWithPath] = Seq.empty,
        methodTypeLookupMap: TypeRep => Generator[any.Method, any.Type] = Map.empty,
        constructorTypeLookupMap: TypeRep => Generator[Constructor, any.Type] = Map.empty,
        classTypeLookupMap: TypeRep => Generator[Class, any.Type] = Map.empty,
        methodReifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[any.Method, any.Expression] = tpe => ???,
        constructorReifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[Constructor, any.Expression] = tpe => ???,
        classReifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[Class, any.Expression] = tpe => ???,
      ): anyOverrides.Project

      def ooCompilationUnit(
        name: Seq[any.Name],
        imports: Seq[any.Import],
        methodTypeLookupMap: TypeRep => Generator[any.Method, any.Type] = Map.empty,
        constructorTypeLookupMap: TypeRep => Generator[Constructor, any.Type] = Map.empty,
        classTypeLookupMap: TypeRep => Generator[Class, any.Type] = Map.empty,
        methodReifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[any.Method, any.Expression] = tpe => ???,
        constructorReifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[Constructor, any.Expression] = tpe => ???,
        classReifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[Class, any.Expression] = tpe => ???,
        classes: Seq[Class] = Seq.empty,
        tests: Seq[any.TestSuite] = Seq.empty
      ): anyOverrides.CompilationUnit
      
      def clsMethod(
        name: any.Name,
        imports: Set[any.Import] = Set.empty,
        statements: Seq[any.Statement] = Seq.empty,
        returnType: Option[any.Type] = Option.empty,
        parameters: Seq[(any.Name, any.Type)] = Seq.empty,
        typeLookupMap: TypeRep => Generator[any.Method, any.Type] = Map.empty,
        reifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[any.Method, any.Expression] = tpe => ???,
        isAbstract: Boolean = false,
        isStatic: Boolean = false,
        isPublic: Boolean = false,
        isOverride: Boolean = false
      ): anyOverrides.Method

      def cls(
        name: any.Name,
        imports: Seq[any.Import] = Seq.empty,
        parents: Seq[any.Type] = Seq.empty,
        implemented: Seq[any.Type] = Seq.empty,
        fields: Seq[Field] = Seq.empty,
        methods: Seq[any.Method] = Seq.empty,
        constructors: Seq[Constructor] = Seq.empty,
        methodTypeLookupMap: TypeRep => Generator[any.Method, any.Type] = Map.empty,
        constructorTypeLookupMap: TypeRep => Generator[Constructor, any.Type] = Map.empty,
        typeLookupMap: TypeRep => Generator[Class, any.Type] = Map.empty,
        methodReifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[any.Method, any.Expression] = tpe => ???,
        constructorReifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[Constructor, any.Expression] = tpe => ???,
        reifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[Class, any.Expression] = tpe => ???,
        isAbstract: Boolean = false,
        isInterface: Boolean = false,
        isStatic: Boolean = false,
      ): Class

      def classBasedTestSuite(underlyingClass: Class, testMarkers: Seq[Boolean]): anyOverrides.TestSuite

      def constructor(
        constructedType: Option[any.Type] = Option.empty,
        imports: Set[any.Import] = Set.empty,
        statements: Seq[any.Statement] = Seq.empty,
        parameters: Seq[(any.Name, any.Type)] = Seq.empty,
        typeLookupMap: TypeRep => Generator[any.Method, any.Type] = Map.empty,
        constructorTypeLookupMap: TypeRep => Generator[Constructor, any.Type] = Map.empty,
        reifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[any.Method, any.Expression] = tpe => ???,
        constructorReifyLookupMap: (tpe: TypeRep) => tpe.HostType => Generator[Constructor, any.Expression] = tpe => ???,
        superInitialization: Option[(any.Type, Seq[any.Expression])] = Option.empty,
        fieldInitializers: Seq[(any.Name, any.Expression)] = Seq.empty,
      ): Constructor

      def field(
        name: any.Name,
        tpe: any.Type,
        init: Option[any.Expression] = Option.empty,
      ): Field

      def memberAccessExpression(
        owner: any.Expression,
        field: any.Name,
      ): MemberAccessExpression

      def objectInstantiationExpression(
        tpe: any.Type,
        constructorArguments: Seq[any.Expression],
        body: Option[Class] = Option.empty
      ): ObjectInstantiationExpression

      def castExpression(
        tpe: any.Type,
        expression: any.Expression
      ): CastExpression

      def instanceOfExpression(
        tpe: any.Type,
        expression: any.Expression
      ): InstanceOfExpression

      def superReferenceExpression(
        parentType: any.Type
      ): SuperReferenceExpression

      def selfReferenceExpression: SelfReferenceExpression

      def classReferenceType(qualifiedClassName: any.Name*): ClassReferenceType
      
      implicit def convert(other: Class): ooFinalTypes.Class = other.getSelfClass
      implicit def convert(other: Constructor): ooFinalTypes.Constructor = other.getSelfConstructor
      implicit def convert(other: Field): ooFinalTypes.Field = other.getSelfField
      implicit def convert(other: MemberAccessExpression): ooFinalTypes.MemberAccessExpression = other.getSelfMemberAccessExpression
      implicit def convert(other: SelfReferenceExpression): ooFinalTypes.SelfReferenceExpression = other.getSelfSelfReferenceExpression
      implicit def convert(other: ObjectInstantiationExpression): ooFinalTypes.ObjectInstantiationExpression = other.getSelfObjectInstantiationExpression
      implicit def convert(other: CastExpression): ooFinalTypes.CastExpression = other.getSelfCastExpression
      implicit def convert(other: InstanceOfExpression): ooFinalTypes.InstanceOfExpression = other.getSelfInstanceOfExpression
      implicit def convert(other: SuperReferenceExpression): ooFinalTypes.SuperReferenceExpression = other.getSelfSuperReferenceExpression
      implicit def convert(other: ClassReferenceType): ooFinalTypes.ClassReferenceType = other.getSelfClassReferenceType
    }
  }

  val finalTypes: oo.anyOverrides.FinalTypes
  val ooFinalTypes: oo.FinalTypes
  val factory: oo.anyOverrides.Factory
  val ooFactory: oo.Factory
}
