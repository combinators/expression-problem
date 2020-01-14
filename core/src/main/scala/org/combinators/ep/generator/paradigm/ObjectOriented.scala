package org.combinators.ep.generator.paradigm

import org.combinators.ep.generator.{AbstractSyntax, Command, Understands}
import Command.Generator
import org.combinators.ep.domain.abstractions.{DataType, TypeRep}

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

case class AddField[Name, Type](name: Name, tpe: Type) extends Command {
  type Result = Unit
}

case class GetField[Name,Expression](name: Name) extends Command {
  type Result = Expression
}

case class InitializeField[Name, Expression](name: Name, value: Expression) extends Command {
  type Result = Unit
}

case class InitializeParent[Expression](arguments: Seq[Expression]) extends Command {
  type Result = Unit
}

case class AddConstructor[ConstructorContext](ctor: Generator[ConstructorContext, Unit]) extends Command {
  type Result = Unit
}

case class InstantiateObject[Type, Expression](tpe: Type, constructorArguments: Seq[Expression]) extends Command {
  type Result = Expression
}

case class CastObject[Type, Expression](tpe: Type, expr: Expression) extends Command {
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

case class SetInterface() extends Command {
  type Result = Unit
}

case class SelfReference[Expression]() extends Command {
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
      AnyParadigm.capabilitiy(AddClass(name, cls))
  }
  val compilationUnitCapabilities: CompilationUnitCapabilities

  trait ClassCapabilities {
    implicit val canDebugInClass: Understands[ClassContext, Debug]
    def debug(tag:String = ""): Generator[ClassContext, Unit] =
      AnyParadigm.capabilitiy(Debug(tag))

    implicit val canAddParentInClass: Understands[ClassContext, AddParent[Type]]
    def addParent(parentClass: Type): Generator[ClassContext, Unit] =
      AnyParadigm.capabilitiy(AddParent(parentClass))

    implicit val canAddImplementedInClass: Understands[ClassContext, AddImplemented[Type]]
    def addImplemented(interface: Type): Generator[ClassContext, Unit] =
      AnyParadigm.capabilitiy(AddImplemented(interface))

    // FIRST REMOVE CAPABILITY
    implicit val canRemoveMethodFromClass: Understands[ClassContext, RemoveMethod[Type, Name]]
    def removeMethod(interface: Type, name:Name): Generator[ClassContext, Unit] =
      AnyParadigm.capabilitiy(RemoveMethod(interface, name))

    implicit val canAddFieldInClass: Understands[ClassContext, AddField[Name, Type]]
    def addField(name: Name, tpe: Type): Generator[ClassContext, Unit] =
      AnyParadigm.capabilitiy(AddField[Name, Type](name, tpe))

    // can get a field (by name) and it becomes an expression by itself
    implicit val canGetFieldInClass: Understands[ClassContext, GetField[Name,Expression]]
    def getField(name: Name): Generator[ClassContext, Expression] =
      AnyParadigm.capabilitiy(GetField[Name,Expression](name))

    implicit val canAddMethodInClass: Understands[ClassContext, AddMethod[MethodBodyContext, Name, Option[Expression]]]
    def addMethod(
        name: Name,
        spec: Generator[MethodBodyContext, Option[Expression]],
        isPublic: Boolean = true): Generator[ClassContext, Unit] =
      AnyParadigm.capabilitiy(AddMethod(name, spec, isPublic))

    def addAbstractMethod(name: Name, spec: Generator[MethodBodyContext, Unit], isPublic: Boolean = true): Generator[ClassContext, Unit] = {
      addMethod(name, spec.flatMap(_ => methodBodyCapabilities.setAbstract()).map(_ => None), isPublic)
    }

    implicit val canAddConstructorInClass: Understands[ClassContext, AddConstructor[ConstructorContext]]
    def addConstructor(ctor: Generator[ConstructorContext, Unit]): Generator[ClassContext, Unit] =
      AnyParadigm.capabilitiy(AddConstructor(ctor))

    implicit val canAddImportInClass: Understands[ClassContext, AddImport[Import]]
    def addImport(imp: Import): Generator[ClassContext, Unit] =
      AnyParadigm.capabilitiy(AddImport(imp))

    implicit val canResolveImportInClass: Understands[ClassContext, ResolveImport[Import, Type]]
    def resolveImport(tpe: Type): Generator[ClassContext, Option[Import]]  =
      AnyParadigm.capabilitiy(ResolveImport[Import, Type](tpe))

    implicit val canSetAbstractInClass: Understands[ClassContext, SetAbstract]
    def setAbstract(): Generator[ClassContext, Unit] =
      AnyParadigm.capabilitiy(SetAbstract())

    implicit val canSetStaticInClass: Understands[ClassContext, SetStatic]
    def setStatic(): Generator[ClassContext, Unit] =
      AnyParadigm.capabilitiy(SetStatic())

    implicit val canSetInterfaceInClass: Understands[ClassContext, SetInterface]
    def setInterface(): Generator[ClassContext, Unit] =
      AnyParadigm.capabilitiy(SetInterface())

    implicit val canTranslateTypeInClass: Understands[ClassContext, ToTargetLanguageType[Type]]
    def toTargetLanguageType(tpe: TypeRep): Generator[ClassContext, Type] =
      AnyParadigm.capabilitiy(ToTargetLanguageType[Type](tpe))

    implicit val canSelfReferenceInClass: Understands[ClassContext, SelfReference[Expression]]
    def selfReference(): Generator[ClassContext, Expression] =
      AnyParadigm.capabilitiy(SelfReference[Expression]())

    implicit val canFindClassInClass: Understands[ClassContext, FindClass[Name, Type]]
    def findClass(qualifiedName: Name*): Generator[ClassContext, Type] =
      AnyParadigm.capabilitiy(FindClass[Name, Type](qualifiedName))

    implicit val canGetFreshNameInClass: Understands[ClassContext, FreshName[Name]]
    def freshName(basedOn: Name): Generator[ClassContext, Name] =
      AnyParadigm.capabilitiy(FreshName[Name](basedOn))
  }
  val classCapabilities: ClassCapabilities

  trait ConstructorCapabilities {
    implicit val canInitializeParentInConstructor: Understands[ConstructorContext, InitializeParent[Expression]]
    def initializeParent(arguments: Seq[Expression]): Generator[ConstructorContext, Unit] =
      AnyParadigm.capabilitiy(InitializeParent(arguments))

    implicit val canCastInConstructor: Understands[ConstructorContext, CastObject[Type, Expression]]
    def castObject(tpe:Type, expr: Expression): Generator[ConstructorContext, Expression] =
      AnyParadigm.capabilitiy(CastObject(tpe, expr))

    implicit val canInitializeFieldInConstructor: Understands[ConstructorContext, InitializeField[Name, Expression]]
    def initializeField(name: Name, value: Expression): Generator[ConstructorContext, Unit] =
      AnyParadigm.capabilitiy(InitializeField(name, value))

    implicit val canAddBlockDefinitionsInConstructor: Understands[ConstructorContext, AddBlockDefinitions[Statement]]
    def addBlockDefinitions(definitions: Seq[Statement]): Generator[ConstructorContext, Unit] =
      AnyParadigm.capabilitiy(AddBlockDefinitions(definitions))

    implicit val canAddImportInConstructor: Understands[ConstructorContext, AddImport[Import]]
    def addImport(imp: Import, modifiers:Int = 0): Generator[ConstructorContext, Unit] =
      AnyParadigm.capabilitiy(AddImport(imp))

    implicit val canResolveImportInConstructor: Understands[ConstructorContext, ResolveImport[Import, Type]]
    def resolveImport(tpe: Type): Generator[ConstructorContext, Option[Import]] =
      AnyParadigm.capabilitiy(ResolveImport[Import, Type](tpe))

    implicit val canInstantiateObjectInConstructor: Understands[ConstructorContext, InstantiateObject[Type, Expression]]
    def instantiateObject(tpe: Type, constructorArguments: Seq[Expression]): Generator[ConstructorContext, Expression] =
      AnyParadigm.capabilitiy(InstantiateObject(tpe, constructorArguments))

    implicit val canApplyInConstructor: Understands[ConstructorContext, Apply[Expression, Expression, Expression]]
    def apply(method: Expression, arguments: Seq[Expression]): Generator[ConstructorContext, Expression] =
      AnyParadigm.capabilitiy(Apply[Expression, Expression, Expression](method, arguments))

    implicit val canGetMemberInConstructor: Understands[ConstructorContext, GetMember[Expression, Name]]
    def getMember(instance: Expression, member: Name): Generator[ConstructorContext, Expression] =
      AnyParadigm.capabilitiy(GetMember(instance, member))

    implicit val canSelfReferenceInConstructor: Understands[ConstructorContext, SelfReference[Expression]]
    def selfReference(): Generator[ConstructorContext, Expression] =
      AnyParadigm.capabilitiy(SelfReference[Expression]())

    implicit val canGetArgumentsInConstructor: Understands[ConstructorContext, GetArguments[Type, Name, Expression]]
    def getArguments(): Generator[ConstructorContext, Seq[(Name, Type, Expression)]] =
      AnyParadigm.capabilitiy(GetArguments[Type, Name, Expression]())

    implicit val canTranslateTypeInConstructor: Understands[ConstructorContext, ToTargetLanguageType[Type]]
    def toTargetLanguageType(tpe: TypeRep): Generator[ConstructorContext, Type] =
      AnyParadigm.capabilitiy(ToTargetLanguageType[Type](tpe))

    implicit def canReifyInConstructor[T]: Understands[ConstructorContext, Reify[T, Expression]]
    def reify[T](tpe: TypeRep.OfHostType[T], elem: T): Generator[ConstructorContext, Expression] =
      AnyParadigm.capabilitiy(Reify[T, Expression](tpe, elem))

    implicit val canSetParametersInConstructor: Understands[ConstructorContext, SetParameters[Name, Type]]
    def setParameters(params: Seq[(Name, Type)]): Generator[ConstructorContext, Unit] =
      AnyParadigm.capabilitiy(SetParameters(params))

    implicit val canGetConstructorInConstructor: Understands[ConstructorContext, GetConstructor[Type, Expression]]
    def getConstructor(tpe: Type): Generator[ConstructorContext, Expression] =
      AnyParadigm.capabilitiy(GetConstructor[Type, Expression](tpe))

    implicit val canFindClassInConstructor: Understands[ConstructorContext, FindClass[Name, Type]]
    def findClass(qualifiedName: Name*): Generator[ConstructorContext, Type] =
      AnyParadigm.capabilitiy(FindClass[Name, Type](qualifiedName))

    implicit val canGetFreshNameInConstructor: Understands[ConstructorContext, FreshName[Name]]
    def freshName(basedOn: Name): Generator[ConstructorContext, Name] =
      AnyParadigm.capabilitiy(FreshName[Name](basedOn))
  }
  val constructorCapabilities: ConstructorCapabilities

  trait MethodBodyCapabilities {
    implicit val canInstantiateObjectInMethod: Understands[MethodBodyContext, InstantiateObject[Type, Expression]]
    def instantiateObject(tpe: Type, constructorArguments: Seq[Expression]): Generator[MethodBodyContext, Expression] =
      AnyParadigm.capabilitiy(InstantiateObject(tpe, constructorArguments))

    implicit val canGetMemberInMethod: Understands[MethodBodyContext, GetMember[Expression, Name]]
    def getMember(instance: Expression, member: Name): Generator[MethodBodyContext, Expression] =
      AnyParadigm.capabilitiy(GetMember(instance, member))

    implicit val canCastInMethod: Understands[MethodBodyContext, CastObject[Type, Expression]]
    def castObject(tpe:Type, expr: Expression): Generator[MethodBodyContext, Expression] =
      AnyParadigm.capabilitiy(CastObject(tpe, expr))

    implicit val canSetAbstractInMethod: Understands[MethodBodyContext, SetAbstract]
    def setAbstract(): Generator[MethodBodyContext, Unit] =
      AnyParadigm.capabilitiy(SetAbstract())

    implicit val canSetStaticInMethod: Understands[MethodBodyContext, SetStatic]
    def setStatic(): Generator[MethodBodyContext, Unit] =
      AnyParadigm.capabilitiy(SetStatic())

    implicit val canSelfReferenceInMethod: Understands[MethodBodyContext, SelfReference[Expression]]
    def selfReference(): Generator[MethodBodyContext, Expression] =
      AnyParadigm.capabilitiy(SelfReference[Expression]())

    implicit val canGetConstructorInMethod: Understands[MethodBodyContext, GetConstructor[Type, Expression]]
    def getConstructor(tpe: Type): Generator[MethodBodyContext, Expression] =
      AnyParadigm.capabilitiy(GetConstructor[Type, Expression](tpe))

    implicit val canFindClassInMethod: Understands[MethodBodyContext, FindClass[Name, Type]]
    def findClass(qualifiedName: Name*): Generator[MethodBodyContext, Type] =
      AnyParadigm.capabilitiy(FindClass[Name, Type](qualifiedName))
  }
  val methodBodyCapabilities: MethodBodyCapabilities

  trait ProjectCapabilities {
    def addClassToProject(classGen: Generator[ClassContext, Unit], qualifiedName: Name* ): Generator[ProjectContext, Unit] = {
      import compilationUnitCapabilities._
      import base.projectContextCapabilities._
      addCompilationUnit(AddClass(qualifiedName.last, classGen).interpret, qualifiedName: _*)
    }

    implicit val canAddTypeLookupForClassesInProject: Understands[ProjectContext, AddTypeLookup[ClassContext, Type]]
    def addTypeLookupForClasses(tpe: TypeRep, lookup: Generator[ClassContext, Type]): Generator[ProjectContext, Unit] =
      AnyParadigm.capabilitiy(AddTypeLookup[ClassContext, Type](tpe, lookup))

    implicit val canAddTypeLookupForConstructorsInProject: Understands[ProjectContext, AddTypeLookup[ConstructorContext, Type]]
    def addTypeLookupForConstructors(tpe: TypeRep, lookup: Generator[ConstructorContext, Type]): Generator[ProjectContext, Unit] =
      AnyParadigm.capabilitiy(AddTypeLookup[ConstructorContext, Type](tpe, lookup))
  }
  val projectCapabilities: ProjectCapabilities

  trait TestCapabilities {
    // helper methods can be added to OO test cases
    implicit val canAddMethodInTest: Understands[TestContext, AddMethod[MethodBodyContext, Name, Option[Expression]]]
    def addMethod(
                   name: Name,
                   spec: Generator[MethodBodyContext, Option[Expression]],
                   isPublic: Boolean = true): Generator[TestContext, Unit] =
      AnyParadigm.capabilitiy(AddMethod(name, spec, isPublic))
  }
  val testCapabilities: TestCapabilities
}

object ObjectOriented {
  type WithBase[B <: AnyParadigm] = ObjectOriented { val base: B }
}