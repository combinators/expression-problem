package org.combinators.cogen.paradigm

import org.combinators.cogen.{Command, Understands, TypeRep}
import Command.Generator

case class AddClass[ClassContext, Name](
    name: Name,
    cls: Generator[ClassContext, Unit]
  ) extends Command {
  type Result = Unit
}

case class AddParent[Type](parentClass: Type) extends Command {
  type Result = Unit
}

case class AddImplemented[Type](interface: Type) extends Command {
  type Result = Unit
}

case class RemoveMethod[Type, Name](interface: Type, name:Name) extends Command {
  type Result = Unit
}

case class AddField[Name, Type, Expression](name: Name, tpe: Type, isMutable: Boolean = true, isVisibleToSubclasses:Boolean = true, initializer:Option[Expression] = Option.empty) extends Command {
  // TODO: investigate why this has isMutable: Boolean = true, isVisibleToSubclasses:Boolean = true,
  type Result = Unit
}

case class GetField[Name,Expression](name: Name) extends Command {
  type Result = Expression
}

case class InitializeField[Name, Expression](name: Name, value: Expression) extends Command {
  type Result = Unit
}

case class InitializeParent[Type, Expression](parent: Type, arguments: Seq[Expression]) extends Command {
  type Result = Unit
}

case class AddConstructor[ConstructorContext](ctor: Generator[ConstructorContext, Unit]) extends Command {
  type Result = Unit
}

/** Supports both traditional instantiation
 * new X (a1,a2)
 * new X (a1, a2) { ... body ... }
 * */
case class InstantiateObject[Type, Expression, ClassContext](tpe: Type, constructorArguments: Seq[Expression], body:Option[Generator[ClassContext,Unit]] = None) extends Command {
  type Result = Expression
}

case class CastObject[Type, Expression](tpe: Type, expr: Expression) extends Command {
  type Result = Expression
}

case class InstanceOfType[Type, Expression](tpe: Type, expr: Expression) extends Command {
  type Result = Expression
}

case class GetMember[Expression, Name](instance: Expression, member: Name) extends Command {
  type Result = Expression
}

case class SetAbstract() extends Command {
  type Result = Unit
}

case class SetStatic() extends Command {
  type Result = Unit
}

case class SetOverride() extends Command {
  type Result = Unit
}

case class SetInterface() extends Command {
  type Result = Unit
}

case class SelfReference[Expression]() extends Command {
  type Result = Expression
}

/** Used for super. or (when tpe exists) then qn.super. */
case class SuperReference[Name, Expression](qualifiedName:Seq[Name] = Seq.empty) extends Command {
  type Result = Expression
}

case class GetConstructor[Type, Expression](tpe: Type) extends Command {
  type Result = Expression
}

case class FindClass[Name, Type](qualifiedName: Seq[Name]) extends Command {
  type Result = Type
}

trait ObjectOriented {
  val base: AnyParadigm
  import base._
  import syntax._

  type ClassContext
  type ConstructorContext

  trait CompilationUnitCapabilities {
    implicit val canAddClassInCompilationUnit: Understands[CompilationUnitContext, AddClass[ClassContext, Name]]
    def addClass(name: Name, cls: Generator[ClassContext, Unit]): Generator[CompilationUnitContext, Unit] =
      AnyParadigm.capability(AddClass(name, cls))
  }
  val compilationUnitCapabilities: CompilationUnitCapabilities

  trait ClassCapabilities {
    implicit val canDebugInClass: Understands[ClassContext, Debug]
    def debug(tag:String = ""): Generator[ClassContext, Unit] =
      AnyParadigm.capability(Debug(tag))

    implicit val canAddTypeLookupForMethodsInClass: Understands[ClassContext, AddTypeLookup[MethodBodyContext, Type]]
    def addTypeLookupForMethods(tpe: TypeRep, lookup: Generator[MethodBodyContext, Type]): Generator[ClassContext, Unit] =
      AnyParadigm.capability(AddTypeLookup[MethodBodyContext, Type](tpe, lookup))

    implicit val canAddTypeLookupForClassesInClass: Understands[ClassContext, AddTypeLookup[ClassContext, Type]]
    def addTypeLookupForClasses(tpe: TypeRep, lookup: Generator[ClassContext, Type]): Generator[ClassContext, Unit] =
      AnyParadigm.capability(AddTypeLookup[ClassContext, Type](tpe, lookup))

    implicit val canAddTypeLookupForConstructorsInClass: Understands[ClassContext, AddTypeLookup[ConstructorContext, Type]]
    def addTypeLookupForConstructors(tpe: TypeRep, lookup: Generator[ConstructorContext, Type]): Generator[ClassContext, Unit] =
      AnyParadigm.capability(AddTypeLookup[ConstructorContext, Type](tpe, lookup))

    implicit val canAddParentInClass: Understands[ClassContext, AddParent[Type]]
    def addParent(parentClass: Type): Generator[ClassContext, Unit] =
      AnyParadigm.capability(AddParent(parentClass))

    implicit val canAddImplementedInClass: Understands[ClassContext, AddImplemented[Type]]
    def addImplemented(interface: Type): Generator[ClassContext, Unit] =
      AnyParadigm.capability(AddImplemented(interface))

    // FIRST REMOVE CAPABILITY
    implicit val canRemoveMethodFromClass: Understands[ClassContext, RemoveMethod[Type, Name]]
    def removeMethod(interface: Type, name:Name): Generator[ClassContext, Unit] =
      AnyParadigm.capability(RemoveMethod(interface, name))

    implicit val canAddFieldInClass: Understands[ClassContext, AddField[Name, Type, Expression]]
    def addField(name: Name, tpe: Type, init:Option[Expression] = Option.empty): Generator[ClassContext, Unit] =
      AnyParadigm.capability(AddField[Name, Type, Expression](name, tpe, initializer = init))

    // can get a field (by name) and it becomes an expression by itself
    implicit val canGetFieldInClass: Understands[ClassContext, GetField[Name,Expression]]
    def getField(name: Name): Generator[ClassContext, Expression] =
      AnyParadigm.capability(GetField[Name,Expression](name))

    implicit val canAddMethodInClass: Understands[ClassContext, AddMethod[MethodBodyContext, Name, Option[Expression]]]
    def addMethod(
        name: Name,
        spec: Generator[MethodBodyContext, Option[Expression]],
        isPublic: Boolean = true,
        isOverride: Boolean = false): Generator[ClassContext, Unit] =
      AnyParadigm.capability(AddMethod(name, spec, isPublic, isOverride))

    def addAbstractMethod(name: Name, spec: Generator[MethodBodyContext, Unit], isPublic: Boolean = true): Generator[ClassContext, Unit] = {
      addMethod(name, spec.flatMap(_ => methodBodyCapabilities.setAbstract()).map(_ => None), isPublic)
    }

    implicit val canAddConstructorInClass: Understands[ClassContext, AddConstructor[ConstructorContext]]
    def addConstructor(ctor: Generator[ConstructorContext, Unit]): Generator[ClassContext, Unit] =
      AnyParadigm.capability(AddConstructor(ctor))

    implicit val canAddImportInClass: Understands[ClassContext, AddImport[Import]]
    def addImport(imp: Import): Generator[ClassContext, Unit] =
      AnyParadigm.capability(AddImport(imp))

    implicit val canResolveImportInClass: Understands[ClassContext, ResolveImport[Import, Type]]
    def resolveImport(tpe: Type): Generator[ClassContext, Option[Import]]  =
      AnyParadigm.capability(ResolveImport[Import, Type](tpe))

    implicit val canSetAbstractInClass: Understands[ClassContext, SetAbstract]
    def setAbstract(): Generator[ClassContext, Unit] =
      AnyParadigm.capability(SetAbstract())

    implicit val canSetStaticInClass: Understands[ClassContext, SetStatic]
    def setStatic(): Generator[ClassContext, Unit] =
      AnyParadigm.capability(SetStatic())
    
    implicit val canSetInterfaceInClass: Understands[ClassContext, SetInterface]
    def setInterface(): Generator[ClassContext, Unit] =
      AnyParadigm.capability(SetInterface())

    implicit val canTranslateTypeInClass: Understands[ClassContext, ToTargetLanguageType[Type]]
    def toTargetLanguageType(tpe: TypeRep): Generator[ClassContext, Type] =
      AnyParadigm.capability(ToTargetLanguageType[Type](tpe))

    implicit val canSelfReferenceInClass: Understands[ClassContext, SelfReference[Expression]]
    def selfReference(): Generator[ClassContext, Expression] =
      AnyParadigm.capability(SelfReference[Expression]())

    implicit val canFindClassInClass: Understands[ClassContext, FindClass[Name, Type]]
    def findClass(qualifiedName: Name*): Generator[ClassContext, Type] =
      AnyParadigm.capability(FindClass[Name, Type](qualifiedName))

    implicit val canGetFreshNameInClass: Understands[ClassContext, FreshName[Name]]
    def freshName(basedOn: Name): Generator[ClassContext, Name] =
      AnyParadigm.capability(FreshName[Name](basedOn))
  }
  val classCapabilities: ClassCapabilities

  trait ConstructorCapabilities {
    implicit val canInitializeParentInConstructor: Understands[ConstructorContext, InitializeParent[Type, Expression]]
    def initializeParent(parent: Type, arguments: Seq[Expression]): Generator[ConstructorContext, Unit] =
      AnyParadigm.capability(InitializeParent(parent, arguments))

    implicit val canCastInConstructor: Understands[ConstructorContext, CastObject[Type, Expression]]
    def castObject(tpe:Type, expr: Expression): Generator[ConstructorContext, Expression] =
      AnyParadigm.capability(CastObject(tpe, expr))

    implicit val canInitializeFieldInConstructor: Understands[ConstructorContext, InitializeField[Name, Expression]]
    def initializeField(name: Name, value: Expression): Generator[ConstructorContext, Unit] =
      AnyParadigm.capability(InitializeField(name, value))

    implicit val canAddBlockDefinitionsInConstructor: Understands[ConstructorContext, AddBlockDefinitions[Statement]]
    def addBlockDefinitions(definitions: Seq[Statement]): Generator[ConstructorContext, Unit] =
      AnyParadigm.capability(AddBlockDefinitions(definitions))

    implicit val canAddImportInConstructor: Understands[ConstructorContext, AddImport[Import]]
    def addImport(imp: Import, modifiers:Int = 0): Generator[ConstructorContext, Unit] =
      AnyParadigm.capability(AddImport(imp))

    implicit val canResolveImportInConstructor: Understands[ConstructorContext, ResolveImport[Import, Type]]
    def resolveImport(tpe: Type): Generator[ConstructorContext, Option[Import]] =
      AnyParadigm.capability(ResolveImport[Import, Type](tpe))

    implicit val canInstantiateObjectInConstructor: Understands[ConstructorContext, InstantiateObject[Type, Expression, ClassContext]]
    def instantiateObject(tpe: Type, constructorArguments: Seq[Expression], body:Option[Generator[ClassContext,Unit]] = None): Generator[ConstructorContext, Expression] =
      AnyParadigm.capability(InstantiateObject(tpe, constructorArguments, body))

    implicit val canApplyInConstructor: Understands[ConstructorContext, Apply[Expression, Expression, Expression]]
    def apply(method: Expression, arguments: Seq[Expression]): Generator[ConstructorContext, Expression] =
      AnyParadigm.capability(Apply[Expression, Expression, Expression](method, arguments))

    implicit val canGetMemberInConstructor: Understands[ConstructorContext, GetMember[Expression, Name]]
    def getMember(instance: Expression, member: Name): Generator[ConstructorContext, Expression] =
      AnyParadigm.capability(GetMember(instance, member))

    implicit val canSelfReferenceInConstructor: Understands[ConstructorContext, SelfReference[Expression]]
    def selfReference(): Generator[ConstructorContext, Expression] =
      AnyParadigm.capability(SelfReference[Expression]())

    implicit val canGetArgumentsInConstructor: Understands[ConstructorContext, GetArguments[Type, Name, Expression]]
    def getArguments(): Generator[ConstructorContext, Seq[(Name, Type, Expression)]] =
      AnyParadigm.capability(GetArguments[Type, Name, Expression]())

    implicit val canTranslateTypeInConstructor: Understands[ConstructorContext, ToTargetLanguageType[Type]]
    def toTargetLanguageType(tpe: TypeRep): Generator[ConstructorContext, Type] =
      AnyParadigm.capability(ToTargetLanguageType[Type](tpe))

    implicit def canReifyInConstructor[T]: Understands[ConstructorContext, Reify[T, Expression]]
    def reify[T](tpe: TypeRep.OfHostType[T], elem: T): Generator[ConstructorContext, Expression] =
      AnyParadigm.capability(Reify[T, Expression](tpe, elem))

    implicit val canSetParametersInConstructor: Understands[ConstructorContext, SetParameters[Name, Type]]
    def setParameters(params: Seq[(Name, Type)]): Generator[ConstructorContext, Unit] =
      AnyParadigm.capability(SetParameters(params))

    implicit val canGetConstructorInConstructor: Understands[ConstructorContext, GetConstructor[Type, Expression]]
    def getConstructor(tpe: Type): Generator[ConstructorContext, Expression] =
      AnyParadigm.capability(GetConstructor[Type, Expression](tpe))
    // TODO: remove

    implicit val canFindClassInConstructor: Understands[ConstructorContext, FindClass[Name, Type]]
    def findClass(qualifiedName: Name*): Generator[ConstructorContext, Type] =
      AnyParadigm.capability(FindClass[Name, Type](qualifiedName))

    implicit val canGetFreshNameInConstructor: Understands[ConstructorContext, FreshName[Name]]
    def freshName(basedOn: Name): Generator[ConstructorContext, Name] =
      AnyParadigm.capability(FreshName[Name](basedOn))
  }
  val constructorCapabilities: ConstructorCapabilities

  trait MethodBodyCapabilities {

    implicit val canInstantiateObjectInMethod: Understands[MethodBodyContext, InstantiateObject[Type, Expression, ClassContext]]
    def instantiateObject(tpe: Type, constructorArguments: Seq[Expression], body:Option[Generator[ClassContext,Unit]] = None): Generator[MethodBodyContext, Expression] =
      AnyParadigm.capability(InstantiateObject(tpe, constructorArguments, body))

    implicit val canGetMemberInMethod: Understands[MethodBodyContext, GetMember[Expression, Name]]
    def getMember(instance: Expression, member: Name): Generator[MethodBodyContext, Expression] =
      AnyParadigm.capability(GetMember(instance, member))

    implicit val canCastInMethod: Understands[MethodBodyContext, CastObject[Type, Expression]]
    def castObject(tpe:Type, expr: Expression): Generator[MethodBodyContext, Expression] =
      AnyParadigm.capability(CastObject(tpe, expr))

    implicit val canInstanceOfTypeInMethod: Understands[MethodBodyContext, InstanceOfType[Type, Expression]]
    def instanceOfType(tpe:Type, expr: Expression): Generator[MethodBodyContext, Expression] =
      AnyParadigm.capability(InstanceOfType(tpe, expr))

    implicit val canSetAbstractInMethod: Understands[MethodBodyContext, SetAbstract]
    def setAbstract(): Generator[MethodBodyContext, Unit] =
      AnyParadigm.capability(SetAbstract())

    implicit val canSetStaticInMethod: Understands[MethodBodyContext, SetStatic]
    def setStatic(): Generator[MethodBodyContext, Unit] =
      AnyParadigm.capability(SetStatic())

    
    // TODO: remove, already there in addMethod
    implicit val canSetOverrideInMethod: Understands[MethodBodyContext, SetOverride]
    def setOverride(): Generator[MethodBodyContext, Unit] =
      AnyParadigm.capability(SetOverride())
    
    implicit val canSelfReferenceInMethod: Understands[MethodBodyContext, SelfReference[Expression]]
    def selfReference(): Generator[MethodBodyContext, Expression] =
      AnyParadigm.capability(SelfReference[Expression]())

    implicit val canSuperReferenceInMethod: Understands[MethodBodyContext, SuperReference[Name,Expression]]
    def superReference(parent:Name*): Generator[MethodBodyContext, Expression] =
      AnyParadigm.capability(SuperReference[Name,Expression](parent))
    //TODO: change parent to be a type

    implicit val canGetConstructorInMethod: Understands[MethodBodyContext, GetConstructor[Type, Expression]]
    def getConstructor(tpe: Type): Generator[MethodBodyContext, Expression] =
      AnyParadigm.capability(GetConstructor[Type, Expression](tpe))
    // TODO: remove this

    implicit val canFindClassInMethod: Understands[MethodBodyContext, FindClass[Name, Type]]
    def findClass(qualifiedName: Name*): Generator[MethodBodyContext, Type] =
      AnyParadigm.capability(FindClass[Name, Type](qualifiedName))
  }
  val methodBodyCapabilities: MethodBodyCapabilities

  trait ProjectCapabilities {
    def addClassToProject(classGen: Generator[ClassContext, Unit], qualifiedName: Name* ): Generator[ProjectContext, Unit] = {
      import compilationUnitCapabilities._
      import base.projectCapabilities._
      addCompilationUnit(AddClass(qualifiedName.last, classGen).interpret, qualifiedName*)
    }

    implicit val canAddTypeLookupForClassesInProject: Understands[ProjectContext, AddTypeLookup[ClassContext, Type]]
    def addTypeLookupForClasses(tpe: TypeRep, lookup: Generator[ClassContext, Type]): Generator[ProjectContext, Unit] =
      AnyParadigm.capability(AddTypeLookup[ClassContext, Type](tpe, lookup))

    implicit val canAddTypeLookupForConstructorsInProject: Understands[ProjectContext, AddTypeLookup[ConstructorContext, Type]]
    def addTypeLookupForConstructors(tpe: TypeRep, lookup: Generator[ConstructorContext, Type]): Generator[ProjectContext, Unit] =
      AnyParadigm.capability(AddTypeLookup[ConstructorContext, Type](tpe, lookup))
  }
  val projectCapabilities: ProjectCapabilities

  trait TestCapabilities {
    // helper methods can be added to OO test cases
    implicit val canAddMethodInTest: Understands[TestContext, AddMethod[MethodBodyContext, Name, Option[Expression]]]
    def addMethod(
                   name: Name,
                   spec: Generator[MethodBodyContext, Option[Expression]],
                   isPublic: Boolean = true): Generator[TestContext, Unit] =
      AnyParadigm.capability(AddMethod(name, spec, isPublic))

    implicit val canAddBlockDefinitionsInTest: Understands[TestContext, AddBlockDefinitions[Statement]]
    def addBlockDefinitions(definitions: Seq[Statement]): Generator[TestContext, Unit] =
      AnyParadigm.capability(AddBlockDefinitions(definitions))

    implicit val canAddFieldInTest: Understands[TestContext, AddField[Name, Type, Expression]]
    def addField(name: Name, tpe: Type, initializer:Option[Expression] = Option.empty): Generator[TestContext, Unit] =
      AnyParadigm.capability(AddField[Name, Type, Expression](name, tpe, initializer = initializer))

    implicit val canInitializeFieldInTest: Understands[TestContext, InitializeField[Name, Expression]]
    def initializeField(name: Name, value: Expression): Generator[TestContext, Unit] =
      AnyParadigm.capability(InitializeField(name, value))

    implicit val canInstantiateObjectInTest: Understands[TestContext, InstantiateObject[Type, Expression, TestContext]]
    def instantiateObject(tpe: Type, constructorArguments: Seq[Expression], body:Option[Generator[TestContext,Unit]] = None): Generator[TestContext, Expression] =
      AnyParadigm.capability(InstantiateObject(tpe, constructorArguments, body))

    implicit val canAddConstructorInTest: Understands[TestContext, AddConstructor[ConstructorContext]]
    def addConstructor(ctor: Generator[ConstructorContext, Unit]): Generator[TestContext, Unit] =
      AnyParadigm.capability(AddConstructor(ctor))

    implicit val canAddImportInTest: Understands[TestContext, AddImport[Import]]
    def addImport(imp: Import): Generator[TestContext, Unit] =
      AnyParadigm.capability(AddImport(imp))

    implicit val canResolveImportInTest: Understands[TestContext, ResolveImport[Import, Type]]
    def resolveImport(tpe: Type): Generator[TestContext, Option[Import]]  =
      AnyParadigm.capability(ResolveImport[Import, Type](tpe))

    implicit val canFindClassInTest: Understands[TestContext, FindClass[Name, Type]]
    def findClass(qualifiedName: Name*): Generator[TestContext, Type] =
      AnyParadigm.capability(FindClass[Name, Type](qualifiedName))

    implicit val canAddImplementedInTest: Understands[TestContext, AddImplemented[Type]]
    def addImplemented(interface: Type): Generator[TestContext, Unit] =
      AnyParadigm.capability(AddImplemented(interface))
  }
  val testCapabilities: TestCapabilities
}

object ObjectOriented {
  type WithBase[B <: AnyParadigm] = ObjectOriented { val base: B }
}