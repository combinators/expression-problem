package org.combinators.ep.generator.paradigm

import org.combinators.ep.generator.{AbstractSyntax, Command, Understands}
import Command.Generator

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
    implicit val canAddImplementedInClass: Understands[ClassContext, AddImplemented[Type]]
    implicit val canAddFieldInClass: Understands[ClassContext, AddField[Type]]
    implicit val canAddMethodInClass: Understands[ClassContext, AddMethod[MethodBodyContext, Option[Expression]]]
    implicit val canAddConstructorInClass: Understands[ClassContext, AddConstructor[ConstructorContext]]
    implicit val canAddImportInClass: Understands[ClassContext, AddImport[Import]]
    implicit val canResolveImportInClass: Understands[ClassContext, ResolveImport[Import, Type]]
    implicit val canSetAbstractInClass: Understands[ClassContext, SetAbstract]
    implicit val canSetInterfaceInClass: Understands[ClassContext, SetInterface]
    implicit val canTranslateTypeInClass: Understands[ClassContext, ToTargetLanguageType[Type]]
    implicit val canSelfReferenceInClass: Understands[ClassContext, SelfReference[Expression]]
    implicit val canFindClassInClass: Understands[ClassContext, FindClass[Type]]
  }
  val classCapabilities: ClassCapabilities

  trait ConstructorCapabilities {
    implicit val canInitializeParentInConstructor: Understands[ConstructorContext, InitializeParent[Expression]]
    implicit val canInitializeFieldInConstructor: Understands[ConstructorContext, InitializeField[Expression]]
    implicit val canAddBlockDefinitionsInConstructor: Understands[ConstructorContext, AddBlockDefinitions[Statement]]
    implicit val canAddImportInConstructor: Understands[ConstructorContext, AddImport[Import]]
    implicit val canResolveImportInConstructor: Understands[ConstructorContext, ResolveImport[Import, Type]]
    implicit val canInstantiateObjectInConstructor: Understands[ConstructorContext, InstantiateObject[Type, Expression]]
    implicit val canApplyInConstructor: Understands[ConstructorContext, Apply[Expression]]
    implicit val canGetMemberInConstructor: Understands[ConstructorContext, GetMember[Expression]]
    implicit val canSelfReferenceInConstructor: Understands[ConstructorContext, SelfReference[Expression]]
    implicit val canGetArgumentsInConstructor: Understands[ConstructorContext, GetArguments[Type, Expression]]
    implicit val canTranslateTypeInConstructor: Understands[ConstructorContext, ToTargetLanguageType[Type]]
    implicit def canReifyInConstructor[T]: Understands[ConstructorContext, Reify[T, Expression]]
    implicit val canSetParametersInConstructor: Understands[ConstructorContext, SetParameters[Type]]
    implicit val canGetConstructorInConstructor: Understands[ConstructorContext, GetConstructor[Type, Expression]]
    implicit val canFindClassInConstructor: Understands[ConstructorContext, FindClass[Type]]
  }
  val constructorCapabilities: ConstructorCapabilities

  trait MethodBodyCapabilities {
    implicit val canInstantiateObjectInMethod: Understands[MethodBodyContext, InstantiateObject[Type, Expression]]
    implicit val canGetMemberInMethod: Understands[MethodBodyContext, GetMember[Expression]]
    implicit val canSetAbstractInMethod: Understands[MethodBodyContext, SetAbstract]
    implicit val canSelfReferenceInMethod: Understands[MethodBodyContext, SelfReference[Expression]]
    implicit val canGetConstructorInMethod: Understands[MethodBodyContext, GetConstructor[Type, Expression]]
    implicit val canFindClassInMethod: Understands[MethodBodyContext, FindClass[Type]]
  }
  val methodBodyCapabilities: MethodBodyCapabilities

  def addClassToProject(name: String, classGen: Generator[ClassContext, Unit]): Generator[ProjectContext, Unit] = {
    import compilationUnitCapabilities._
    import base.projectContextCapabilities._
    AddCompilationUnit(name, AddClass(name, classGen).interpret).interpret
  }

  def addAbstractMethod(name: String, spec: Generator[MethodBodyContext, Unit], isPublic: Boolean = true): Generator[ClassContext, Unit] = {
    import classCapabilities._
    import methodBodyCapabilities._
    AddMethod[MethodBodyContext, Option[Expression]](name,
      spec.flatMap(_ => SetAbstract().interpret).map(_ => None),
      isPublic
    ).interpret
  }
}

object ObjectOriented {
  type WithBase[B <: AnyParadigm] = ObjectOriented { val base: B }
}