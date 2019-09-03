package org.combinators.ep.generator.paradigm

import org.combinators.ep.generator.{AbstractSyntax, Command, Understands}
import Command.Generator

case class AddClass[ClassContext, Type](
    name: String,
    parents: Seq[Type],
    cls: Generator[ClassContext, Unit]
  ) extends Command {
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

abstract class ObjectOriented[S <: AbstractSyntax](override val syntax: S) extends AnyParadigm[S](syntax) {
  import syntax._

  type ClassContext
  type ConstructorContext

  implicit val canAddClassInCompilationUnit: Understands[CompilationUnitContext, AddClass[ClassContext, Type]]

  implicit val canAddFieldInClass: Understands[ClassContext, AddField[Type, Expression]]
  implicit val canAddMethodInClass: Understands[ClassContext, AddMethod[MethodBodyContext, Type, Expression]]
  implicit val canAddConstructorInClass: Understands[ClassContext, AddConstructor[ConstructorContext, Unit]]
  implicit val canInstantiateObjectInClass: Understands[ClassContext, InstantiateObject[Type, Expression]]

  implicit val canInitializeParentInConstructor: Understands[ConstructorContext, InitializeParent[Expression]]
  implicit val canInitializeFieldInConstructor: Understands[ConstructorContext, InitializeField[Expression]]
  implicit val canAddBlockDefinitionsInConstructor: Understands[ConstructorContext, AddBlockDefinitions[Statement]]
  implicit val canAddImportInConstructor: Understands[ConstructorContext, AddImport[Import]]
  implicit val canInstantiateObjectInConstructor: Understands[ConstructorContext, InstantiateObject[Type, Expression]]

  implicit val canInstantiateObjectInMethod: Understands[MethodBodyContext, InstantiateObject[Type, Expression]]

  def addClassToProject(name: String, parents: Seq[Type])(classGen: Generator[ClassContext, Unit]): Generator[ProjectContext, Unit] = {
    AddCompilationUnit(name, AddClass(name, parents, classGen).interpret).interpret
  }
}
