package org.combinators.ep.language.inbetween

import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.generator.Command
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.language.inbetween.any.{CompilationUnit, Project}

package object oo {
  trait FinalTypes extends any.FinalTypes {
    type Class
    type Constructor
    type Field
    type MemberAccessExpression <: Expression
    type SelfReferenceExpression <: Expression
    type ObjectInstantiationExpression <: Expression
    type CastExpression <: Expression
    type InstanceOfExpression <: Expression
    type SuperReferenceExpression <: Expression
    type ClassReferenceType <: Type
  }

  trait Project[FT <: FinalTypes] extends any.Project[FT] with Factory[FT] {
    def methodTypeLookupMap: Map[TypeRep, Generator[any.Method[FT], any.Type[FT]]] = Map.empty
    def constructorTypeLookupMap: Map[TypeRep, Generator[Constructor[FT], any.Type[FT]]] = Map.empty
    def classTypeLookupMap: Map[TypeRep, Generator[Class[FT], any.Type[FT]]] = Map.empty

    override def addTypeLookupForMethods(tpeRep: TypeRep, tpe: Generator[any.Method[FT], any.Type[FT]]): any.Project[FT] =
      copyAsProjectWithTypeLookups(methodTypeLookupMap = this.methodTypeLookupMap + (tpeRep -> tpe))

    override def copy(
      compilationUnits: Set[any.CompilationUnit[FT]]
    ): Project[FT] = copyAsProjectWithTypeLookups(compilationUnits)

    def copyAsProjectWithTypeLookups(
      compilationUnits: Set[any.CompilationUnit[FT]] = this.compilationUnits,
      methodTypeLookupMap: Map[TypeRep, Generator[any.Method[FT], any.Type[FT]]] = this.methodTypeLookupMap,
      constructorTypeLookupMap: Map[TypeRep, Generator[Constructor[FT], any.Type[FT]]] = this.constructorTypeLookupMap,
      classTypeLookupMap: Map[TypeRep, Generator[Class[FT], any.Type[FT]]] = this.classTypeLookupMap
    ): Project[FT] = ooProject(compilationUnits, methodTypeLookupMap, constructorTypeLookupMap, classTypeLookupMap)
  }

  trait CompilationUnit[FT <: FinalTypes] extends any.CompilationUnit[FT] with Factory[FT] {
    def classes: Seq[Class[FT]] = Seq.empty
    def methodTypeLookupMap: Map[TypeRep, Generator[any.Method[FT], any.Type[FT]]] = Map.empty
    def constructorTypeLookupMap: Map[TypeRep, Generator[Constructor[FT], any.Type[FT]]] = Map.empty
    def classTypeLookupMap: Map[TypeRep, Generator[Class[FT], any.Type[FT]]] = Map.empty

    override def initializeInProject(project: any.Project[FT]): any.CompilationUnit[FT] =
      copyAsCompilationUnitWithClasses(classTypeLookupMap = project.classTypeLookupMap,
        constructorTypeLookupMap = project.constructorTypeLookupMap,
        methodTypeLookupMap = project.methodTypeLookupMap
      )

    override def copy(
      name: Seq[any.Name[FT]] = this.name,
      imports: Seq[any.Import[FT]] = this.imports
    ): CompilationUnit[FT] = copyAsCompilationUnitWithClasses(name, imports)

    def copyAsCompilationUnitWithClasses(
      name: Seq[any.Name[FT]] = this.name,
      imports: Seq[any.Import[FT]] = this.imports,
      methodTypeLookupMap: Map[TypeRep, Generator[any.Method[FT], any.Type[FT]]] = this.methodTypeLookupMap,
      constructorTypeLookupMap: Map[TypeRep, Generator[Constructor[FT], any.Type[FT]]] = this.constructorTypeLookupMap,
      classTypeLookupMap: Map[TypeRep, Generator[Class[FT], any.Type[FT]]] = this.classTypeLookupMap,
      classes: Seq[Class[FT]] = this.classes
    ): CompilationUnit[FT] = compilationUnit(name, imports, methodTypeLookupMap, constructorTypeLookupMap, classTypeLookupMap, classes)
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
    def typeLookupMap: Map[TypeRep, any.Type[FT]] = Map.empty
    def methodTypeLookupMap: Map[TypeRep, Generator[any.Method[FT], any.Type[FT]]] = Map.empty
    def constructorTypeLookupMap: Map[TypeRep, Generator[Constructor[FT], any.Type[FT]]] = Map.empty
    def isAbstract: Boolean = false
    def isInterface: Boolean = false
    def isStatic: Boolean = false

    def toTargetLanguageType(tpe: TypeRep): any.Type[FT] = typeLookupMap(tpe)

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

    def resolveImport(tpe: any.Type[FT]): Option[any.Import[FT]]
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
      methodTypeLookupMap: Map[TypeRep, Generator[any.Method[FT], any.Type[FT]]] = this.methodTypeLookupMap,
      constructorTypeLookupMap: Map[TypeRep, Generator[Constructor[FT], any.Type[FT]]] = this.constructorTypeLookupMap,
      typeLookupMap: Map[TypeRep, any.Type[FT]] = this.typeLookupMap,
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

  trait Method[FT <: FinalTypes] extends any.Method[FT] with Factory[FT] {
    def isAbstract: Boolean = false
    def isStatic: Boolean = false
    def isPublic: Boolean = false
    def isOverride: Boolean = false

    def findClass(qualifiedName: any.Name[FT]*): any.Type[FT]

    override def copy(
      name: any.Name[FT] = this.name,
      imports: Set[any.Import[FT]] = this.imports,
      statements: Seq[any.Statement[FT]] = this.statements,
      returnType: Option[any.Type[FT]] = this.returnType,
      parameters: Seq[(any.Name[FT], any.Type[FT])] = this.parameters,
      typeLookupMap: Map[TypeRep, any.Type[FT]] = this.typeLookupMap
    ): Method[FT] = clsMethod(name, imports, statements, returnType, parameters, typeLookupMap, isAbstract, isStatic, isPublic, isOverride)

    def copyAsClsMethod(
      name: any.Name[FT] = this.name,
      imports: Set[any.Import[FT]] = this.imports,
      statements: Seq[any.Statement[FT]] = this.statements,
      returnType: Option[any.Type[FT]] = this.returnType,
      parameters: Seq[(any.Name[FT], any.Type[FT])] = this.parameters,
      typeLookupMap: Map[TypeRep, any.Type[FT]] = this.typeLookupMap,
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
    def getSelfConstructor: finalTypes.Constructor

    override def isAbstract: Boolean = false
    override def isOverride: Boolean = false

    def constructedType: Option[any.Type[FT]] = Option.empty
    def superInitialization: Option[(any.Type[FT], Seq[any.Expression[FT]])] = Option.empty
    def fieldInitializers: Seq[(any.Name[FT], any.Expression[FT])] = Seq.empty

    override def addTypeLookup(tpeRep: TypeRep, tpe: any.Type[FT]): Constructor[FT] = {
      copyAsConstructor(typeLookupMap = typeLookupMap.updated(tpeRep, tpe))
    }

    override def  copyAsClsMethod(
      name: any.Name[FT] = this.name,
      imports: Set[any.Import[FT]] = this.imports,
      statements: Seq[any.Statement[FT]] = this.statements,
      returnType: Option[any.Type[FT]] = this.returnType,
      parameters: Seq[(any.Name[FT], any.Type[FT])] = this.parameters,
      typeLookupMap: Map[TypeRep, any.Type[FT]] = this.typeLookupMap,
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
      typeLookupMap: Map[TypeRep, any.Type[FT]] = this.typeLookupMap,
      superInitialization: Option[(any.Type[FT], Seq[any.Expression[FT]])] = this.superInitialization,
      fieldInitializers: Seq[(any.Name[FT], any.Expression[FT])] = this.fieldInitializers,
    ): Constructor[FT] = constructor(constructedType, imports, statements, parameters, typeLookupMap, superInitialization, fieldInitializers)
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
    ): ClassReferenceType[FT] = classReferenceType(qualifiedClassName:_*)
  }

  trait Factory[FT <: FinalTypes] extends any.Factory[FT] {

    override def project(compilationUnits: Set[any.CompilationUnit[FT]]): Project[FT] =
      ooProject(compilationUnits = compilationUnits, methodTypeLookupMap=Map.empty, constructorTypeLookupMap=Map.empty, classTypeLookupMap=Map.empty)

    def ooProject(
      compilationUnits: Set[any.CompilationUnit[FT]] = Set.empty,
      methodTypeLookupMap: Map[TypeRep, Generator[any.Method[FT], any.Type[FT]]] = Map.empty,
      constructorTypeLookupMap: Map[TypeRep, Generator[Constructor[FT], any.Type[FT]]] = Map.empty,
      classTypeLookupMap: Map[TypeRep, Generator[Class[FT], any.Type[FT]]] = Map.empty
    ): any.Project[FT]


    override def compilationUnit(name: Seq[any.Name[FT]], imports: Seq[any.Import[FT]]): CompilationUnit[FT] =
      compilationUnit(name, imports, methodTypeLookupMap=Map.empty, constructorTypeLookupMap=Map.empty, classTypeLookupMap=Map.empty, classes=Seq.empty)

    def compilationUnit(
      name: Seq[any.Name[FT]],
      imports: Seq[any.Import[FT]],
      methodTypeLookupMap: Map[TypeRep, Generator[any.Method[FT], any.Type[FT]]] = Map.empty,
      constructorTypeLookupMap: Map[TypeRep, Generator[Constructor[FT], any.Type[FT]]] = Map.empty,
      classTypeLookupMap: Map[TypeRep, Generator[Class[FT], any.Type[FT]]] = Map.empty,
      classes: Seq[Class[FT]] = Seq.empty): CompilationUnit[FT]

    override def method(
      name: any.Name[FT],
      imports: Set[any.Import[FT]] = Set.empty,
      statements: Seq[any.Statement[FT]] = Seq.empty,
      returnType: Option[any.Type[FT]] = Option.empty,
      parameters: Seq[(any.Name[FT], any.Type[FT])] = Seq.empty,
      typeLookupMap: Map[TypeRep, any.Type[FT]] = Map.empty
    ): Method[FT] = clsMethod(name, imports, statements, returnType, parameters, typeLookupMap, false, false, false, false)

    def clsMethod(
      name: any.Name[FT],
      imports: Set[any.Import[FT]] = Set.empty,
      statements: Seq[any.Statement[FT]] = Seq.empty,
      returnType: Option[any.Type[FT]] = Option.empty,
      parameters: Seq[(any.Name[FT], any.Type[FT])] = Seq.empty,
      typeLookupMap: Map[TypeRep, any.Type[FT]] = Map.empty,
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
      methodTypeLookupMap: Map[TypeRep, Generator[any.Method[FT], any.Type[FT]]] = Map.empty,
      constructorTypeLookupMap: Map[TypeRep, Generator[Constructor[FT], any.Type[FT]]] = Map.empty,
      typeLookupMap: Map[TypeRep, any.Type[FT]] = Map.empty,
      isAbstract: Boolean = false,
      isInterface: Boolean = false,
      isStatic: Boolean = false,
    ): Class[FT]

    def constructor(
      constructedType: Option[any.Type[FT]] = Option.empty,
      imports: Set[any.Import[FT]] = Set.empty,
      statements: Seq[any.Statement[FT]] = Seq.empty,
      parameters: Seq[(any.Name[FT], any.Type[FT])] = Seq.empty,
      typeLookupMap: Map[TypeRep, any.Type[FT]] = Map.empty,
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