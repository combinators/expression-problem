package org.combinators.ep.generator.paradigm

import org.combinators.ep.generator.{AbstractSyntax, Command, Understands}
import Command.Generator
import org.combinators.ep.domain.abstractions.TypeRep

case class AddClass[ClassContext](
    name: String,
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

case class AddField[Type](name: String, tpe: Type) extends Command {
  type Result = Unit
}

case class InitializeField[Expression](name: String, value: Expression) extends Command {
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

case class GetMember[Expression](instance: Expression, member: String) extends Command {
  type Result = Expression
}

case class SetAbstract() extends Command {
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

case class FindClass[Type](name: String) extends Command {
  type Result = Type
}

trait ObjectOriented {
  val base: AnyParadigm
  import base._
  import syntax._

  type ClassContext
  type ConstructorContext

  trait CompilationUnitCapabilities {
    implicit val canAddClassInCompilationUnit: Understands[CompilationUnitContext, AddClass[ClassContext]]
  }
  val compilationUnitCapabilities: CompilationUnitCapabilities

  trait ClassCapabilities {
    implicit val canAddParentInClass: Understands[ClassContext, AddParent[Type]]
    def addParent(parentClass: Type): Generator[ClassContext, Unit] =
      AnyParadigm.capabilitiy(AddParent(parentClass))

    implicit val canAddImplementedInClass: Understands[ClassContext, AddImplemented[Type]]
    def addImplementedClass(interface: Type): Generator[ClassContext, Unit] =
      AnyParadigm.capabilitiy(AddImplemented(interface))

    implicit val canAddFieldInClass: Understands[ClassContext, AddField[Type]]
    def addField(name: String, tpe: Type): Generator[ClassContext, Unit] =
      AnyParadigm.capabilitiy(AddField(name, tpe))

    implicit val canAddMethodInClass: Understands[ClassContext, AddMethod[MethodBodyContext, Option[Expression]]]
    def addMethod(
        name: String,
        spec: Generator[MethodBodyContext, Option[Expression]],
        isPublic: Boolean = true): Generator[ClassContext, Unit] =
      AnyParadigm.capabilitiy(AddMethod(name, spec, isPublic))

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

    implicit val canSetInterfaceInClass: Understands[ClassContext, SetInterface]
    def setInterface(): Generator[ClassContext, Unit] =
      AnyParadigm.capabilitiy(SetInterface())

    implicit val canTranslateTypeInClass: Understands[ClassContext, ToTargetLanguageType[Type]]
    def toTargetLanguageType(tpe: TypeRep): Generator[ClassContext, Type] =
      AnyParadigm.capabilitiy(ToTargetLanguageType[Type](tpe))

    implicit val canSelfReferenceInClass: Understands[ClassContext, SelfReference[Expression]]
    def selfReference(): Generator[ClassContext, Expression] =
      AnyParadigm.capabilitiy(SelfReference[Expression]())

    implicit val canFindClassInClass: Understands[ClassContext, FindClass[Type]]
    def findClass(name: String): Generator[ClassContext, Type] =
      AnyParadigm.capabilitiy(FindClass[Type](name))
  }
  val classCapabilities: ClassCapabilities

  trait ConstructorCapabilities {
    implicit val canInitializeParentInConstructor: Understands[ConstructorContext, InitializeParent[Expression]]
    def initializeParent(arguments: Seq[Expression]): Generator[ConstructorContext, Unit] =
      AnyParadigm.capabilitiy(InitializeParent(arguments))

    implicit val canInitializeFieldInConstructor: Understands[ConstructorContext, InitializeField[Expression]]
    def initializeField(name: String, value: Expression): Generator[ConstructorContext, Unit] =
      AnyParadigm.capabilitiy(InitializeField(name, value))

    implicit val canAddBlockDefinitionsInConstructor: Understands[ConstructorContext, AddBlockDefinitions[Statement]]
    def addBlockDefinitions(definitions: Seq[Statement]): Generator[ConstructorContext, Unit] =
      AnyParadigm.capabilitiy(AddBlockDefinitions(definitions))

    implicit val canAddImportInConstructor: Understands[ConstructorContext, AddImport[Import]]
    def addImport(imp: Import): Generator[ConstructorContext, Unit] =
      AnyParadigm.capabilitiy(AddImport(imp))

    implicit val canResolveImportInConstructor: Understands[ConstructorContext, ResolveImport[Import, Type]]
    def resolveImport(tpe: Type): Generator[ConstructorContext, Option[Import]] =
      AnyParadigm.capabilitiy(ResolveImport[Import, Type](tpe))

    implicit val canInstantiateObjectInConstructor: Understands[ConstructorContext, InstantiateObject[Type, Expression]]
    def instantiateObject(tpe: Type, constructorArguments: Seq[Expression]): Generator[ConstructorContext, Expression] =
      AnyParadigm.capabilitiy(InstantiateObject(tpe, constructorArguments))

    implicit val canApplyInConstructor: Understands[ConstructorContext, Apply[Expression]]
    def apply(method: Expression, arguments: Seq[Expression]): Generator[ConstructorContext, Expression] =
      AnyParadigm.capabilitiy(Apply(method, arguments))

    implicit val canGetMemberInConstructor: Understands[ConstructorContext, GetMember[Expression]]
    def getMember(instance: Expression, member: String): Generator[ConstructorContext, Expression] =
      AnyParadigm.capabilitiy(GetMember(instance, member))

    implicit val canSelfReferenceInConstructor: Understands[ConstructorContext, SelfReference[Expression]]
    def selfReference(): Generator[ConstructorContext, Expression] =
      AnyParadigm.capabilitiy(SelfReference[Expression]())

    implicit val canGetArgumentsInConstructor: Understands[ConstructorContext, GetArguments[Type, Expression]]
    def getArguments(): Generator[ConstructorContext, Seq[(String, Type, Expression)]] =
      AnyParadigm.capabilitiy(GetArguments[Type, Expression]())

    implicit val canTranslateTypeInConstructor: Understands[ConstructorContext, ToTargetLanguageType[Type]]
    def toTargetLanguageType(tpe: TypeRep): Generator[ConstructorContext, Type] =
      AnyParadigm.capabilitiy(ToTargetLanguageType[Type](tpe))

    implicit def canReifyInConstructor[T]: Understands[ConstructorContext, Reify[T, Expression]]
    def reify[T](tpe: TypeRep.OfHostType[T], elem: T): Generator[ConstructorContext, Expression] =
      AnyParadigm.capabilitiy(Reify[T, Expression](tpe, elem))

    implicit val canSetParametersInConstructor: Understands[ConstructorContext, SetParameters[Type]]
    def setParameters(params: Seq[(String, Type)]): Generator[ConstructorContext, Unit] =
      AnyParadigm.capabilitiy(SetParameters(params))

    implicit val canGetConstructorInConstructor: Understands[ConstructorContext, GetConstructor[Type, Expression]]
    def getConstructor(tpe: Type): Generator[ConstructorContext, Expression] =
      AnyParadigm.capabilitiy(GetConstructor[Type, Expression](tpe))

    implicit val canFindClassInConstructor: Understands[ConstructorContext, FindClass[Type]]
    def findClass(name: String): Generator[ConstructorContext, Type] =
      AnyParadigm.capabilitiy(FindClass[Type](name))
  }
  val constructorCapabilities: ConstructorCapabilities

  trait MethodBodyCapabilities {
    implicit val canInstantiateObjectInMethod: Understands[MethodBodyContext, InstantiateObject[Type, Expression]]
    def instantiateObject(tpe: Type, constructorArguments: Seq[Expression]): Generator[MethodBodyContext, Expression] =
      AnyParadigm.capabilitiy(InstantiateObject(tpe, constructorArguments))

    implicit val canGetMemberInMethod: Understands[MethodBodyContext, GetMember[Expression]]
    def getMember(instance: Expression, member: String): Generator[MethodBodyContext, Expression] =
      AnyParadigm.capabilitiy(GetMember(instance, member))

    implicit val canSetAbstractInMethod: Understands[MethodBodyContext, SetAbstract]
    def setAbstract(): Generator[MethodBodyContext, Unit] =
      AnyParadigm.capabilitiy(SetAbstract())

    implicit val canSelfReferenceInMethod: Understands[MethodBodyContext, SelfReference[Expression]]
    def selfReference(): Generator[MethodBodyContext, Expression] =
      AnyParadigm.capabilitiy(SelfReference[Expression]())

    implicit val canGetConstructorInMethod: Understands[MethodBodyContext, GetConstructor[Type, Expression]]
    def getConstructor(tpe: Type): Generator[MethodBodyContext, Expression] =
      AnyParadigm.capabilitiy(GetConstructor[Type, Expression](tpe))

    implicit val canFindClassInMethod: Understands[MethodBodyContext, FindClass[Type]]
    def findClass(name: String): Generator[MethodBodyContext, Type] =
      AnyParadigm.capabilitiy(FindClass[Type](name))
  }
  val methodBodyCapabilities: MethodBodyCapabilities

  def addClassToProject(name: String, classGen: Generator[ClassContext, Unit]): Generator[ProjectContext, Unit] = {
    import compilationUnitCapabilities._
    import base.projectContextCapabilities._
    addCompilationUnit(name, AddClass(name, classGen).interpret)
  }

  def addAbstractMethod(name: String, spec: Generator[MethodBodyContext, Unit], isPublic: Boolean = true): Generator[ClassContext, Unit] = {
    import classCapabilities._
    addMethod(name, spec.flatMap(_ => methodBodyCapabilities.setAbstract()).map(_ => None), isPublic)
  }
}

object ObjectOriented {
  type WithBase[B <: AnyParadigm] = ObjectOriented { val base: B }
}