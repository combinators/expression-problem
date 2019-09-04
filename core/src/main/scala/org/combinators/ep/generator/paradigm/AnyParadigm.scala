package org.combinators.ep.generator.paradigm

import org.combinators.ep.domain.abstractions.{DataType, DataTypeCase, TypeRep}
import org.combinators.ep.domain.instances.{DataTypeInstance, InstanceRep}
import org.combinators.ep.generator.{AbstractSyntax, Command, Understands}
import Command._
import cats.implicits._

/** Adds a compilation unit */
case class AddCompilationUnit[CompilationUnitContext](name: String, unit: Generator[CompilationUnitContext, Unit])
    extends Command {
  type Result = Unit
}

/** Adds the given import. */
case class AddImport[Import](imp: Import) extends Command {
  type Result = Unit
}

/** Adds the given definitions to a block of statements. */
case class AddBlockDefinitions[Statement](definitions: Seq[Statement]) extends Command {
  type Result = Unit
}

/** Translates the Scala representation of a type to target language specific code for referring to it. */
case class ToTargetLanguageType[Type](tpe: TypeRep) extends Command {
  type Result = Type
}

/** Adds a method using the body generator's resulting expression as return value. */
case class AddMethod[MethodBodyContext, Expression](
   name: String,
   spec: Generator[MethodBodyContext, Expression],
   isPublic: Boolean = true
  ) extends Command {
  type Result = Unit
}

case class SetReturnType[Type](tpe: Type) extends Command {
  type Result = Unit
}

case class SetParameters[Type](params: Seq[(String, Type)]) extends Command {
  type Result = Unit
}

case class GetArguments[Type, Expression]() extends Command {
  type Result = Seq[(String, Type, Expression)]
}

case class AddTestSuite[TestContext](name: String, suite: Generator[TestContext, Unit]) extends Command {
  type Result = Unit
}
case class AddTestCase[MethodBodyContext](name: String, code: Generator[MethodBodyContext, Unit]) extends Command {
  type Result = Unit
}

case class Apply[T](method: T, arguments: Seq[T]) extends Command {
  type Result = T
}

case class ResolveImport[Import, T](forElem: T) extends Command {
  type Result = Option[Import]
}

trait AnyParadigm {
  val syntax: AbstractSyntax

  import syntax._

  type ProjectContext
  type CompilationUnitContext
  type TestContext
  type MethodBodyContext

  trait ProjectContextCapabilities {
    implicit val canAddCompilationUnitInProject: Understands[ProjectContext, AddCompilationUnit[CompilationUnitContext]]
  }
  val projectContextCapabilities: ProjectContextCapabilities

  trait CompilationUnitCapabilities {
    implicit val canAddImportInCompilationUnit: Understands[CompilationUnitContext, AddImport[Import]]
    implicit val canAddTestSuiteInCompilationUnit: Understands[CompilationUnitContext, AddTestSuite[TestContext]]
  }
  val compilationUnitCapabilities: CompilationUnitCapabilities

  trait MethodBodyCapabilities {
    implicit val canAddImportInMethodBody: Understands[MethodBodyContext, AddImport[Import]]
    implicit val canAddBlockDefinitionsInMethodBody: Understands[MethodBodyContext, AddBlockDefinitions[Statement]]
    implicit val canSetReturnTypeInMethodBody: Understands[MethodBodyContext, SetReturnType[Type]]
    implicit val canSetParametersInMethodBody: Understands[MethodBodyContext, SetParameters[Type]]
    implicit val canTransformTypeInMethodBody: Understands[MethodBodyContext, ToTargetLanguageType[Type]]
    implicit val canApplyInMethodBody: Understands[MethodBodyContext, Apply[Expression]]
    implicit val canGetArgumentsInMethodBody: Understands[MethodBodyContext, GetArguments[Type, Expression]]
  }
  val methodBodyCapabilities: MethodBodyCapabilities

  implicit val canAddTestCaseInTest: Understands[TestContext, AddTestCase[MethodBodyContext]]

  /** Creates an empty project */
  def emptyProject(name: String): ProjectContext

  /** Returns code to instantiate the given data type case, filling in `args` for its parameters. */
  def instantiate[Expression](baseTpe: DataType, tpeCase: DataTypeCase, args: Expression*): Generator[MethodBodyContext, Expression]

  /** Returns code to instantiate the given Scala model of a domain specific type. */
  def instantiate(baseType: DataType, inst: DataTypeInstance): Generator[MethodBodyContext, Expression] = {
    for {
      attributeInstances <- inst.attributeInstances.toList.map(reify).sequence[Generator[MethodBodyContext, *], Expression]
      result <- instantiate(baseType, inst.tpeCase, attributeInstances: _*)
    } yield result
  }

  /** Converts the given Scala value into code representing it in the target language. */
  def reify[T](tpe: TypeRep.OfHostType[T], value: T): Generator[MethodBodyContext, Expression]

  /** Converts a Scala model of an instance of any representable type into code. */
  def reify(inst: InstanceRep): Generator[MethodBodyContext, Expression] = {
    (inst.tpe, inst.inst) match {
      case (TypeRep.DataType(baseTpe), domInst: DataTypeInstance) => instantiate(baseTpe, domInst)
      case (tpe, inst) => reify[tpe.HostType](tpe, inst.asInstanceOf[tpe.HostType])
      case _ => throw new scala.NotImplementedError(s"No rule to compile instantiations of ${inst.tpe}.")
    }
  }
}

object AnyParadigm {
  type WithSyntax[S <: AbstractSyntax] = AnyParadigm { val syntax: S }
}