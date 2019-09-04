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

case class AddField[Type, Expression](name: String, tpe: Type) extends Command {
  type Result = Unit
}

case class InitializeField[Expression](name: String, value: Expression) extends Command {
  type Result = Unit
}

case class InitializeParent[Expression](arguments: Seq[Expression]) extends Command {
  type Result = Unit
}

case class AddConstructor[ConstructorContext, Type](
    parameters: Seq[(String, Type)],
    ctor: Generator[ConstructorContext, Unit]
  ) extends Command {
  type Result = Unit
}

case class InstantiateObject[Type, Expression](tpe: Type, constructorArguments: Seq[Expression]) extends Command {
  type Result = Expression
}

case class GetMember[Expression](instance: Expression, member: String) extends Command {
  type Result = Expression
}

trait ObjectOriented {
  val base: AnyParadigm
  import base._
  import syntax._

  type ClassContext
  type ConstructorContext

  implicit val canAddClassInCompilationUnit: Understands[CompilationUnitContext, AddClass[ClassContext]]

  implicit val canAddParentInClass: Understands[ClassContext, AddParent[Type]]
  implicit val canAddImplementedInClass: Understands[ClassContext, AddImplemented[Type]]
  implicit val canAddFieldInClass: Understands[ClassContext, AddField[Type, Expression]]
  implicit val canAddMethodInClass: Understands[ClassContext, AddMethod[MethodBodyContext, Expression]]
  implicit val canAddConstructorInClass: Understands[ClassContext, AddConstructor[ConstructorContext, Unit]]
  implicit val canAddImportInClass: Understands[ClassContext, AddImport[Import]]
  implicit val canResolveImportInClass: Understands[ClassContext, ResolveImport[Import, Type]]

  implicit val canInitializeParentInConstructor: Understands[ConstructorContext, InitializeParent[Expression]]
  implicit val canInitializeFieldInConstructor: Understands[ConstructorContext, InitializeField[Expression]]
  implicit val canAddBlockDefinitionsInConstructor: Understands[ConstructorContext, AddBlockDefinitions[Statement]]
  implicit val canAddImportInConstructor: Understands[ConstructorContext, AddImport[Import]]
  implicit val canResolveImportInConstructor: Understands[ConstructorContext, ResolveImport[Import, Type]]
  implicit val canInstantiateObjectInConstructor: Understands[ConstructorContext, InstantiateObject[Type, Expression]]
  implicit val canApplyInConstructor: Understands[ConstructorContext, Apply[Expression]]
  implicit val canGetMemberInConstructor: Understands[ConstructorContext, GetMember[Expression]]

  implicit val canInstantiateObjectInMethod: Understands[MethodBodyContext, InstantiateObject[Type, Expression]]
  implicit val canGetMemberInMethod: Understands[MethodBodyContext, GetMember[Expression]]
  implicit val canResolveImportInMethod: Understands[MethodBodyContext, ResolveImport[Import, Type]]

  def addClassToProject(name: String)(classGen: Generator[ClassContext, Unit]): Generator[ProjectContext, Unit] = {
    AddCompilationUnit(name, AddClass(name, classGen).interpret).interpret
  }
}

object ObjectOriented {
  type WithBase[B <: AnyParadigm] = ObjectOriented { val base: B }
}