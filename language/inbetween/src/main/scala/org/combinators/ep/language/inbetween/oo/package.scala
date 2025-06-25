package org.combinators.ep.language.inbetween   /*DI:LI:AI*/

import org.combinators.cogen.abstractions.TypeRep
import org.combinators.ep.generator.Command.Generator

package object oo {
  trait FinalTypes extends any.FinalTypes {
    type Class
    type Constructor
    type Field
    type MemberAccessExpression <: super.Expression
    type SelfReferenceExpression <: super.Expression
    type ObjectInstantiationExpression <: super.Expression
    type CastExpression <: super.Expression
    type InstanceOfExpression <: super.Expression
    type SuperReferenceExpression <: super.Expression
    type ClassReferenceType <: Type
  }

  trait Project[FT <: FinalTypes] extends any.Project[FT] with Factory[FT] {
    def methodTypeLookupMap: TypeRep => Generator[any.Method[FT], any.Type[FT]] = Map.empty
    def constructorTypeLookupMap: TypeRep => Generator[Constructor[FT], any.Type[FT]] = Map.empty
    def classTypeLookupMap: TypeRep => Generator[Class[FT], any.Type[FT]] = Map.empty

    override def addTypeLookupsForMethods(lookups: TypeRep => Option[Generator[any.Method[FT], any.Type[FT]]]): any.Project[FT] =
      copyAsProjectWithTypeLookups(methodTypeLookupMap = (tpeRep: TypeRep) => lookups(tpeRep).getOrElse(this.methodTypeLookupMap(tpeRep)))

    def addTypeLookupsForConstructors(lookups: TypeRep => Option[Generator[oo.Constructor[FT], any.Type[FT]]]): any.Project[FT] =
      copyAsProjectWithTypeLookups(constructorTypeLookupMap = (tpeRep: TypeRep) => lookups(tpeRep).getOrElse(this.constructorTypeLookupMap(tpeRep)))

    def addTypeLookupsForClasses(lookups: TypeRep => Option[Generator[oo.Class[FT], any.Type[FT]]]): any.Project[FT] =
      copyAsProjectWithTypeLookups(classTypeLookupMap = (tpeRep: TypeRep) => lookups(tpeRep).getOrElse(this.classTypeLookupMap(tpeRep)))

    override def copy(
      compilationUnits: Set[any.CompilationUnit[FT]]
    ): any.Project[FT] = copyAsProjectWithTypeLookups(compilationUnits)

    def copyAsProjectWithTypeLookups(
      compilationUnits: Set[any.CompilationUnit[FT]] = this.compilationUnits,
      methodTypeLookupMap: TypeRep => Generator[any.Method[FT], any.Type[FT]] = this.methodTypeLookupMap,
      constructorTypeLookupMap: TypeRep => Generator[Constructor[FT], any.Type[FT]] = this.constructorTypeLookupMap,
      classTypeLookupMap: TypeRep => Generator[Class[FT], any.Type[FT]] = this.classTypeLookupMap
    ): Project[FT] = ooProject(compilationUnits, methodTypeLookupMap, constructorTypeLookupMap, classTypeLookupMap)
  }

  trait CompilationUnit[FT <: FinalTypes] extends any.CompilationUnit[FT] with Factory[FT] {
    def classes: Seq[Class[FT]] = Seq.empty
    def methodTypeLookupMap: TypeRep => Generator[any.Method[FT], any.Type[FT]] = Map.empty
    def constructorTypeLookupMap: TypeRep => Generator[Constructor[FT], any.Type[FT]] = Map.empty
    def classTypeLookupMap: TypeRep => Generator[Class[FT], any.Type[FT]] = Map.empty

    def addTypeLookupsForMethods(lookups: TypeRep => Option[Generator[any.Method[FT], any.Type[FT]]]): any.CompilationUnit[FT] =
      copyAsCompilationUnitWithClasses(methodTypeLookupMap = (tpeRep: TypeRep) => lookups(tpeRep).getOrElse(this.methodTypeLookupMap(tpeRep)))

    def addTypeLookupsForConstructors(lookups: TypeRep => Option[Generator[oo.Constructor[FT], any.Type[FT]]]): any.CompilationUnit[FT] =
      copyAsCompilationUnitWithClasses(constructorTypeLookupMap = (tpeRep: TypeRep) => lookups(tpeRep).getOrElse(this.constructorTypeLookupMap(tpeRep)))

    def addTypeLookupsForClasses(lookups: TypeRep => Option[Generator[oo.Class[FT], any.Type[FT]]]): any.CompilationUnit[FT] =
      copyAsCompilationUnitWithClasses(classTypeLookupMap = (tpeRep: TypeRep) => lookups(tpeRep).getOrElse(this.classTypeLookupMap(tpeRep)))

    override def initializeInProject(project: any.Project[FT]): any.CompilationUnit[FT] = {
      val withLookups = copyAsCompilationUnitWithClasses(classTypeLookupMap = project.classTypeLookupMap,
        constructorTypeLookupMap = project.constructorTypeLookupMap,
        methodTypeLookupMap = project.methodTypeLookupMap
      )
      withLookups.copyAsCompilationUnitWithClasses(
        tests = withLookups.tests.map(_.initializeInCompilationUnit(withLookups))
      )
    }

    override def copy(
      name: Seq[any.Name[FT]] = this.name,
      imports: Seq[any.Import[FT]] = this.imports,
      tests: Seq[any.TestSuite[FT]] = this.tests,
    ): any.CompilationUnit[FT] = copyAsCompilationUnitWithClasses(name, imports, tests = tests)

    def copyAsCompilationUnitWithClasses(
      name: Seq[any.Name[FT]] = this.name,
      imports: Seq[any.Import[FT]] = this.imports,
      methodTypeLookupMap: TypeRep => Generator[any.Method[FT], any.Type[FT]] = this.methodTypeLookupMap,
      constructorTypeLookupMap: TypeRep => Generator[Constructor[FT], any.Type[FT]] = this.constructorTypeLookupMap,
      classTypeLookupMap: TypeRep => Generator[Class[FT], any.Type[FT]] = this.classTypeLookupMap,
      classes: Seq[Class[FT]] = this.classes,
      tests: Seq[any.TestSuite[FT]] = this.tests,
    ): CompilationUnit[FT] = ooCompilationUnit(name, imports, methodTypeLookupMap, constructorTypeLookupMap, classTypeLookupMap, classes, tests)
  }

  trait Class[FT <: FinalTypes] extends Factory[FT] {
    def getSelfClass: finalTypes.Class

    def name: any.Name[FT]
    def imports: Seq[any.Import[FT]] = Seq.empty
    def parents: Seq[any.Type[FT]] = Seq.empty
    def implemented: Seq[any.Type[FT]] = Seq.empty
    def fields: Seq[Field[FT]] = Seq.empty
    def methods: Seq[any.Method[FT]] = Seq.empty
    def constructors: Seq[Constructor[FT]] = Seq.empty
    def methodTypeLookupMap: TypeRep => Generator[any.Method[FT], any.Type[FT]] = Map.empty
    def constructorTypeLookupMap: TypeRep => Generator[Constructor[FT], any.Type[FT]] = Map.empty
    def typeLookupMap: TypeRep => Generator[Class[FT], any.Type[FT]] = Map.empty
    def isAbstract: Boolean = false
    def isInterface: Boolean = false
    def isStatic: Boolean = false

    def toTargetLanguageType(tpe: TypeRep): Generator[Class[FT], any.Type[FT]] = typeLookupMap(tpe)

    def addTypeLookupsForMethods(lookups: TypeRep => Option[Generator[any.Method[FT], any.Type[FT]]]): oo.Class[FT] =
      copy(methodTypeLookupMap = (tpeRep: TypeRep) => lookups(tpeRep).getOrElse(this.methodTypeLookupMap(tpeRep)))

    def addTypeLookupsForConstructors(lookups: TypeRep => Option[Generator[oo.Constructor[FT], any.Type[FT]]]): oo.Class[FT] =
      copy(constructorTypeLookupMap = (tpeRep: TypeRep) => lookups(tpeRep).getOrElse(this.constructorTypeLookupMap(tpeRep)))

    def addTypeLookupsForClasses(lookups: TypeRep => Option[Generator[Class[FT], any.Type[FT]]]): oo.Class[FT] = {
      copy(typeLookupMap = (tpeRep: TypeRep) => lookups(tpeRep).getOrElse(this.typeLookupMap(tpeRep)))
    }

    def addParent(parent: any.Type[FT]): Class[FT] =
      copy(parents = (this.parents :+ parent).distinct)

    def addImplemented(impl: any.Type[FT]): Class[FT] =
      copy(implemented = (this.implemented :+ impl).distinct)

    def addField(field: Field[FT]): Class[FT] =
      copy(fields = this.fields :+ field) // TODO: make distinct by name

    def getField(name: any.Name[FT]): Expression[FT] =
      memberAccessExpression(selfReferenceExpression, name)

    def addMethod(method: Method[FT]): Class[FT] =
      copy(methods = this.methods :+ method)

    def addConstructor(constructor: Constructor[FT]): Class[FT] =
      copy(constructors = this.constructors :+ constructor)

    def resolveImport(tpe: any.Type[FT]): Seq[any.Import[FT]]
    def findClass(qualifiedName: any.Name[FT]*): any.Type[FT]
    def getFreshName(basedOn: any.Name[FT]): any.Name[FT]

    def copy(
      name: any.Name[FT] = this.name,
      imports: Seq[any.Import[FT]] = this.imports,
      parents: Seq[any.Type[FT]] = this.parents,
      implemented: Seq[any.Type[FT]] = this.implemented,
      fields: Seq[Field[FT]] = this.fields,
      methods: Seq[any.Method[FT]] = this.methods,
      constructors: Seq[Constructor[FT]] = this.constructors,
      methodTypeLookupMap: TypeRep => Generator[any.Method[FT], any.Type[FT]] = this.methodTypeLookupMap,
      constructorTypeLookupMap: TypeRep => Generator[Constructor[FT], any.Type[FT]] = this.constructorTypeLookupMap,
      typeLookupMap: TypeRep => Generator[Class[FT], any.Type[FT]] = this.typeLookupMap,
      isAbstract: Boolean = this.isAbstract,
      isInterface: Boolean = this.isInterface,
      isStatic: Boolean = this.isStatic,
    ): Class[FT] = cls(
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
      isAbstract = isAbstract,
      isInterface = isInterface,
      isStatic = isStatic,
    )
  }

  trait TestSuite[FT <: FinalTypes] extends any.TestSuite[FT] with Factory[FT] {
    def underlyingClass: Class[FT]
    def testMarkers: Seq[Boolean]

    override def name: any.Name[FT] = underlyingClass.name
    override def tests: Seq[any.Method[FT]] = underlyingClass.methods.zip(testMarkers).filter{case (m, isTest) => isTest}.map(_._1)
    override def methodTypeLookupMap: TypeRep => Generator[any.Method[FT], any.Type[FT]] = underlyingClass.methodTypeLookupMap

    override def initializeInCompilationUnit(compilationUnit: any.CompilationUnit[FT]): any.TestSuite[FT] =
      copyAsClassBasedTestSuite(
        underlyingClass = underlyingClass.copy(
          constructorTypeLookupMap = compilationUnit.constructorTypeLookupMap,
          methodTypeLookupMap = compilationUnit.methodTypeLookupMap,
          typeLookupMap = compilationUnit.classTypeLookupMap,          
        ),
        testMarkers
      )

    override def copy(
      name: any.Name[FT] = this.name,
      tests: Seq[any.Method[FT]] = this.tests,
      methodTypeLookupMap: TypeRep => Generator[any.Method[FT], any.Type[FT]] = this.methodTypeLookupMap,
    ): any.TestSuite[FT] = {
      val helpers = underlyingClass.methods.zip(this.testMarkers).filter{case (m, isTest) => !isTest}.map(_._1)
      val testMarkers = Seq.fill(helpers.size)(false) ++ Seq.fill(tests.size)(true)

      copyAsClassBasedTestSuite(
        underlyingClass = this.underlyingClass.copy(
          name = name,
          methods = helpers ++ tests,
          methodTypeLookupMap = methodTypeLookupMap
        ),
        testMarkers
      )
    }

    def copyAsClassBasedTestSuite(
      underlyingClass: Class[FT] = this.underlyingClass,
      testMarkers: Seq[Boolean] = this.testMarkers
    ): TestSuite[FT] = classBasedTestSuite(underlyingClass, testMarkers)

  }

  trait Method[FT <: FinalTypes] extends any.Method[FT] with Factory[FT] {
    def isAbstract: Boolean = false
    def isStatic: Boolean = false
    def isPublic: Boolean = false
    def isOverride: Boolean = false

    def findClass(qualifiedName: any.Name[FT]*): any.Type[FT]

    def addTestExpressions(exprs: Seq[any.Expression[FT]]): any.Method[FT]

    override def copy(
      name: any.Name[FT] = this.name,
      imports: Set[any.Import[FT]] = this.imports,
      statements: Seq[any.Statement[FT]] = this.statements,
      returnType: Option[any.Type[FT]] = this.returnType,
      parameters: Seq[(any.Name[FT], any.Type[FT])] = this.parameters,
      typeLookupMap: TypeRep => Generator[any.Method[FT], any.Type[FT]] = this.typeLookupMap
    ): any.Method[FT] = clsMethod(name, imports, statements, returnType, parameters, typeLookupMap, isAbstract, isStatic, isPublic, isOverride)

    def copyAsClsMethod(
      name: any.Name[FT] = this.name,
      imports: Set[any.Import[FT]] = this.imports,
      statements: Seq[any.Statement[FT]] = this.statements,
      returnType: Option[any.Type[FT]] = this.returnType,
      parameters: Seq[(any.Name[FT], any.Type[FT])] = this.parameters,
      typeLookupMap: TypeRep => Generator[any.Method[FT], any.Type[FT]] = this.typeLookupMap,
      isAbstract: Boolean = this.isAbstract,
      isStatic: Boolean = this.isStatic,
      isPublic: Boolean = this.isPublic,
      isOverride: Boolean = this.isOverride,
    ): Method[FT] = clsMethod(name, imports, statements, returnType, parameters, typeLookupMap, isAbstract, isStatic, isPublic, isOverride)
  }

  trait Expression[FT <: FinalTypes] extends any.Expression[FT] with Factory[FT] {

  }

  trait MemberAccessExpression[FT <: FinalTypes] extends Expression[FT] {
    def getSelfMemberAccessExpression: finalTypes.MemberAccessExpression

    def owner: any.Expression[FT]
    def field: any.Name[FT]

    def copy(
      owner: any.Expression[FT] = this.owner,
      field: any.Name[FT] = this.field,
    ): MemberAccessExpression[FT] = memberAccessExpression(owner = owner, field = field)
  }

  trait SelfReferenceExpression[FT <: FinalTypes] extends Expression[FT] {
    def getSelfSelfReferenceExpression: finalTypes.SelfReferenceExpression
  }

  trait SuperReferenceExpression[FT <: FinalTypes] extends Expression[FT] {
    def getSelfSuperReferenceExpression: finalTypes.SuperReferenceExpression

    def parentType: any.Type[FT]

    def copy(
      parentType: any.Type[FT] = this.parentType,
    ): SuperReferenceExpression[FT] = superReferenceExpression(parentType)
  }

  trait ObjectInstantiationExpression[FT <: FinalTypes] extends Expression[FT] {
    def getSelfObjectInstantiationExpression: finalTypes. ObjectInstantiationExpression

    def tpe: any.Type[FT]
    def constructorArguments: Seq[any.Expression[FT]]
    def body: Option[Class[FT]] = Option.empty

    def copy(
      tpe: any.Type[FT] = this.tpe,
      constructorArguments: Seq[any.Expression[FT]] = this.constructorArguments,
      body: Option[Class[FT]] = this.body
    ): ObjectInstantiationExpression[FT] = objectInstantiationExpression(tpe, constructorArguments, body)
  }

  trait CastExpression[FT <: FinalTypes] extends Expression[FT] {
    def getSelfCastExpression: finalTypes.CastExpression

    def tpe: any.Type[FT]
    def expression: any.Expression[FT]

    def copy(
      tpe: any.Type[FT] = this.tpe,
      expression: any.Expression[FT] = this.expression
    ): CastExpression[FT] = castExpression(tpe, expression)
  }

  trait InstanceOfExpression[FT <: FinalTypes] extends Expression[FT] {
    def getSelfInstanceOfExpression: finalTypes.InstanceOfExpression

    def tpe: any.Type[FT]
    def expression: any.Expression[FT]

    def copy(
      tpe: any.Type[FT] = this.tpe,
      expression: any.Expression[FT] = this.expression
    ): InstanceOfExpression[FT] = instanceOfExpression(tpe, expression)
  }

  trait Constructor[FT <: FinalTypes] extends Method[FT] {
    def constructorTypeLookupMap: TypeRep => Generator[Constructor[FT], any.Type[FT]] = Map.empty
    def getSelfConstructor: finalTypes.Constructor

    override def isAbstract: Boolean = false
    override def isOverride: Boolean = false

    def constructedType: Option[any.Type[FT]] = Option.empty
    def superInitialization: Option[(any.Type[FT], Seq[any.Expression[FT]])] = Option.empty
    def fieldInitializers: Seq[(any.Name[FT], any.Expression[FT])] = Seq.empty

    def addConstructorTypeLookups(lookups: TypeRep => Option[Generator[Constructor[FT], any.Type[FT]]]): Constructor[FT] = {
      copyAsConstructor(constructorTypeLookupMap = (tpeRep: TypeRep) => lookups(tpeRep).getOrElse(this.constructorTypeLookupMap(tpeRep)))
    }
    def toTargetLanguageTypeInConstructor(tpe: TypeRep): Generator[Constructor[FT], any.Type[FT]] = constructorTypeLookupMap(tpe)

    override def  copyAsClsMethod(
      name: any.Name[FT] = this.name,
      imports: Set[any.Import[FT]] = this.imports,
      statements: Seq[any.Statement[FT]] = this.statements,
      returnType: Option[any.Type[FT]] = this.returnType,
      parameters: Seq[(any.Name[FT], any.Type[FT])] = this.parameters,
      typeLookupMap: TypeRep => Generator[any.Method[FT], any.Type[FT]] = this.typeLookupMap,
      isAbstract: Boolean = this.isAbstract,
      isStatic: Boolean = this.isStatic,
      isPublic: Boolean = this.isPublic,
      isOverride: Boolean = this.isOverride,
    ): Method[FT] = copyAsConstructor(this.constructedType, imports, statements, parameters, typeLookupMap)

    def copyAsConstructor(
      constructedType: Option[any.Type[FT]] = this.constructedType,
      imports: Set[any.Import[FT]] = this.imports,
      statements: Seq[any.Statement[FT]] = this.statements,
      parameters: Seq[(any.Name[FT], any.Type[FT])] = this.parameters,
      typeLookupMap: TypeRep => Generator[any.Method[FT], any.Type[FT]] = this.typeLookupMap,
      constructorTypeLookupMap: TypeRep => Generator[Constructor[FT],any.Type[FT]] = this.constructorTypeLookupMap,
      superInitialization: Option[(any.Type[FT], Seq[any.Expression[FT]])] = this.superInitialization,
      fieldInitializers: Seq[(any.Name[FT], any.Expression[FT])] = this.fieldInitializers,
    ): Constructor[FT] = constructor(constructedType, imports, statements, parameters, typeLookupMap, constructorTypeLookupMap, superInitialization, fieldInitializers)
  }

  trait Field[FT <: FinalTypes] extends Factory[FT] {
    def getSelfField: finalTypes.Field

    def name: any.Name[FT]
    def tpe: any.Type[FT]
    def init: Option[any.Expression[FT]] = Option.empty

    def copy(
      name: any.Name[FT] = this.name,
      tpe: any.Type[FT] = this.tpe,
      init: Option[any.Expression[FT]] = this.init,
    ): Field[FT] = field(name, tpe, init)
  }

  trait ClassReferenceType[FT <: FinalTypes] extends any.Type[FT] with Factory[FT] {
    def getSelfClassReferenceType: finalTypes.ClassReferenceType

    def qualifiedClassName: Seq[any.Name[FT]]

    def copy(
      qualifiedClassName: Seq[any.Name[FT]] = this.qualifiedClassName
    ): ClassReferenceType[FT] = classReferenceType(qualifiedClassName*)
  }

  trait Factory[FT <: FinalTypes] extends any.Factory[FT] {

    override def project(compilationUnits: Set[any.CompilationUnit[FT]]): any.Project[FT] =
      ooProject(compilationUnits = compilationUnits, methodTypeLookupMap=Map.empty, constructorTypeLookupMap=Map.empty, classTypeLookupMap=Map.empty)


    def ooProject(
      compilationUnits: Set[any.CompilationUnit[FT]] = Set.empty,
      methodTypeLookupMap: TypeRep => Generator[any.Method[FT], any.Type[FT]] = Map.empty,
      constructorTypeLookupMap: TypeRep => Generator[Constructor[FT], any.Type[FT]] = Map.empty,
      classTypeLookupMap: TypeRep => Generator[Class[FT], any.Type[FT]] = Map.empty
    ): any.Project[FT]


    override def compilationUnit(name: Seq[any.Name[FT]], imports: Seq[any.Import[FT]], tests: Seq[any.TestSuite[FT]]): any.CompilationUnit[FT] =
      ooCompilationUnit(name, imports, methodTypeLookupMap=Map.empty, constructorTypeLookupMap=Map.empty, classTypeLookupMap=Map.empty, classes=Seq.empty, tests = tests)

    def ooCompilationUnit(
      name: Seq[any.Name[FT]],
      imports: Seq[any.Import[FT]],
      methodTypeLookupMap: TypeRep => Generator[any.Method[FT], any.Type[FT]] = Map.empty,
      constructorTypeLookupMap: TypeRep => Generator[Constructor[FT], any.Type[FT]] = Map.empty,
      classTypeLookupMap: TypeRep => Generator[Class[FT], any.Type[FT]] = Map.empty,
      classes: Seq[Class[FT]] = Seq.empty,
      tests: Seq[any.TestSuite[FT]] = Seq.empty): CompilationUnit[FT]

    override def method(
      name: any.Name[FT],
      imports: Set[any.Import[FT]] = Set.empty,
      statements: Seq[any.Statement[FT]] = Seq.empty,
      returnType: Option[any.Type[FT]] = Option.empty,
      parameters: Seq[(any.Name[FT], any.Type[FT])] = Seq.empty,
      typeLookupMap: TypeRep => Generator[any.Method[FT], any.Type[FT]] = Map.empty
    ): any.Method[FT] = clsMethod(name, imports, statements, returnType, parameters, typeLookupMap, isAbstract = false, isStatic = false, isPublic = false, isOverride = false)

    def clsMethod(
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
    ): Method[FT]

    def cls(
      name: any.Name[FT],
      imports: Seq[any.Import[FT]] = Seq.empty,
      parents: Seq[any.Type[FT]] = Seq.empty,
      implemented: Seq[any.Type[FT]] = Seq.empty,
      fields: Seq[Field[FT]] = Seq.empty,
      methods: Seq[any.Method[FT]] = Seq.empty,
      constructors: Seq[Constructor[FT]] = Seq.empty,
      methodTypeLookupMap: TypeRep => Generator[any.Method[FT], any.Type[FT]] = Map.empty,
      constructorTypeLookupMap: TypeRep => Generator[Constructor[FT], any.Type[FT]] = Map.empty,
      typeLookupMap: TypeRep => Generator[Class[FT], any.Type[FT]] = Map.empty,
      isAbstract: Boolean = false,
      isInterface: Boolean = false,
      isStatic: Boolean = false,
    ): Class[FT]

    def testSuite(name: any.Name[FT], tests: Seq[any.Method[FT]], methodTypeLookupMap: TypeRep => Generator[any.Method[FT], any.Type[FT]] = Map.empty): TestSuite[FT] =
      classBasedTestSuite(cls(name = name, methods = tests, methodTypeLookupMap = methodTypeLookupMap), Seq.fill(tests.size)(true))

    def classBasedTestSuite(underlyingClass: Class[FT], testMarkers: Seq[Boolean]): TestSuite[FT]

    def constructor(
      constructedType: Option[any.Type[FT]] = Option.empty,
      imports: Set[any.Import[FT]] = Set.empty,
      statements: Seq[any.Statement[FT]] = Seq.empty,
      parameters: Seq[(any.Name[FT], any.Type[FT])] = Seq.empty,
      typeLookupMap: TypeRep => Generator[any.Method[FT], any.Type[FT]] = Map.empty,
      constructorTypeLookupMap: TypeRep => Generator[Constructor[FT], any.Type[FT]] = Map.empty,
      superInitialization: Option[(any.Type[FT], Seq[any.Expression[FT]])] = Option.empty,
      fieldInitializers: Seq[(any.Name[FT], any.Expression[FT])] = Seq.empty,
    ): Constructor[FT]

    def field(
      name: any.Name[FT],
      tpe: any.Type[FT],
      init: Option[any.Expression[FT]] = Option.empty,
    ): Field[FT]

    def memberAccessExpression(
      owner: any.Expression[FT],
      field: any.Name[FT],
    ): MemberAccessExpression[FT]

    def objectInstantiationExpression(
      tpe: any.Type[FT],
      constructorArguments: Seq[any.Expression[FT]],
      body: Option[Class[FT]] = Option.empty
    ): ObjectInstantiationExpression[FT]

    def castExpression(
      tpe: any.Type[FT],
      expression: any.Expression[FT]
    ): CastExpression[FT]

    def instanceOfExpression(
      tpe: any.Type[FT],
      expression: any.Expression[FT]
    ): InstanceOfExpression[FT]

    def superReferenceExpression(
      parentType: any.Type[FT]
    ): SuperReferenceExpression[FT]

    def selfReferenceExpression: SelfReferenceExpression[FT]

    def classReferenceType(qualifiedClassName: any.Name[FT]*): ClassReferenceType[FT]

    implicit def convert(other: any.Project[FT]): Project[FT]
    implicit def convert(other: any.CompilationUnit[FT]): CompilationUnit[FT]
    implicit def convert(other: any.Method[FT]): Method[FT]
    implicit def convert(other: any.TestSuite[FT]): TestSuite[FT]
    implicit def convert(other: Class[FT]): Class[FT]
    implicit def convert(other: Constructor[FT]): Constructor[FT]
    implicit def convert(other: Field[FT]): Field[FT]
    implicit def convert(other: MemberAccessExpression[FT]): MemberAccessExpression[FT]
    implicit def convert(other: SelfReferenceExpression[FT]): SelfReferenceExpression[FT]
    implicit def convert(other: ObjectInstantiationExpression[FT]): ObjectInstantiationExpression[FT]
    implicit def convert(other: CastExpression[FT]): CastExpression[FT]
    implicit def convert(other: InstanceOfExpression[FT]): InstanceOfExpression[FT]
    implicit def convert(other: SuperReferenceExpression[FT]): SuperReferenceExpression[FT]
    implicit def convert(other: ClassReferenceType[FT]): ClassReferenceType[FT]
  }

}