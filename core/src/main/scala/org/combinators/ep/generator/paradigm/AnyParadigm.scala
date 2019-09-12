package org.combinators.ep.generator.paradigm

import org.combinators.ep.domain.abstractions.{DataType, DataTypeCase, TypeRep}
import org.combinators.ep.domain.instances.{DataTypeInstance, InstanceRep}
import org.combinators.ep.generator.{AbstractSyntax, Command, FileWithPath, Understands}
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

/** Converts the given Scala value into code representing it in the target language. */
case class Reify[T, Expression](tpe: TypeRep.OfHostType[T], value: T) extends Command {
  type Result = Expression
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
case class AddTestCase[MethodBodyContext, Expression](name: String, code: Generator[MethodBodyContext, Seq[Expression]]) extends Command {
  type Result = Unit
}

case class Apply[F, S, R](functional: F, arguments: Seq[S]) extends Command {
  type Result = R
}

case class ResolveImport[Import, T](forElem: T) extends Command {
  type Result = Option[Import]
}

case class DeclareVariable[Type, Init, Statement](name: String, tpe: Type, initialization: Init) extends Command {
  type Result = Statement
}

case class IfThenElse[Expression, MandatoryBlock, Block, R](
    condition: Expression,
    ifBranch: MandatoryBlock,
    elseIfBranches: Seq[(Expression, MandatoryBlock)],
    elseBranch: Block) extends Command {
  type Result = R
}

case class AddTypeLookup[Ctxt, Type](tpe: TypeRep, lookup: Generator[Ctxt, Type]) extends Command {
  type Result = Unit
}

trait AnyParadigm {
  val syntax: AbstractSyntax

  import syntax._

  /** Widest project context. */
  type ProjectContext

  /** Individual source files that reflection compilation units. */
  type CompilationUnitContext

  /**
   * For testing, a different context is necessary. This ultimately depends on the language paradigm
   * For example, object-oriented requires a class while functional paradigm uses a CompilationUnit
   */
  type TestContext

  /** Allows clean ability to capture dependencies (i.e., imports) within a given Method Body. */
  type MethodBodyContext

  /** The overall project stores the CompilationUnits which can be added to it. */
  trait ProjectContextCapabilities {
    implicit val canAddCompilationUnitInProject: Understands[ProjectContext, AddCompilationUnit[CompilationUnitContext]]
    def addCompilationUnit(name: String, unit: Generator[CompilationUnitContext, Unit]): Generator[ProjectContext, Unit] =
      AnyParadigm.capabilitiy(AddCompilationUnit(name, unit))

    implicit val canAddTypeLookupForMethodsInProject: Understands[ProjectContext, AddTypeLookup[MethodBodyContext, Type]]
    def addTypeLookupForMethods(tpe: TypeRep, lookup: Generator[MethodBodyContext, Type]): Generator[ProjectContext, Unit] =
      AnyParadigm.capabilitiy(AddTypeLookup[MethodBodyContext, Type](tpe, lookup))
  }
  val projectContextCapabilities: ProjectContextCapabilities

  /** Each CompilationUnit may have external import dependencies and associated test cases. */
  trait CompilationUnitCapabilities {
    implicit val canAddImportInCompilationUnit: Understands[CompilationUnitContext, AddImport[Import]]
    def addImport(imp: Import): Generator[CompilationUnitContext, Unit] =
      AnyParadigm.capabilitiy(AddImport(imp))

    implicit val canAddTestSuiteInCompilationUnit: Understands[CompilationUnitContext, AddTestSuite[TestContext]]
    def addTestSuite(name: String, suite: Generator[TestContext, Unit]): Generator[CompilationUnitContext, Unit] =
      AnyParadigm.capabilitiy(AddTestSuite[TestContext](name, suite))
  }
  val compilationUnitCapabilities: CompilationUnitCapabilities

  /**
   * A method declaration can:
   *
   *   - request a new import in the enclosing context
   *   - contain a block of statements
   *   - has a return type
   *   - may have parameters
   *   - can convert TypeRep into local type in language
   *   - TODO: explain reify
   *
   */
  trait MethodBodyCapabilities {
    implicit val canAddImportInMethodBody: Understands[MethodBodyContext, AddImport[Import]]
    def addImport(imp: Import): Generator[MethodBodyContext, Unit] =
      AnyParadigm.capabilitiy(AddImport(imp))

    implicit val canAddBlockDefinitionsInMethodBody: Understands[MethodBodyContext, AddBlockDefinitions[Statement]]
    def addBlockDefinitions(definitions: Seq[Statement]): Generator[MethodBodyContext, Unit] =
      AnyParadigm.capabilitiy(AddBlockDefinitions[Statement](definitions))

    implicit val canSetReturnTypeInMethodBody: Understands[MethodBodyContext, SetReturnType[Type]]
    def setReturnType(tpe: Type): Generator[MethodBodyContext, Unit] =
      AnyParadigm.capabilitiy(SetReturnType(tpe))

    implicit val canSetParametersInMethodBody: Understands[MethodBodyContext, SetParameters[Type]]
    def setParameters(params: Seq[(String, Type)]): Generator[MethodBodyContext, Unit] =
      AnyParadigm.capabilitiy(SetParameters(params))

    implicit val canTransformTypeInMethodBody: Understands[MethodBodyContext, ToTargetLanguageType[Type]]
    def toTargetLanguageType(tpe: TypeRep): Generator[MethodBodyContext, Type] =
      AnyParadigm.capabilitiy(ToTargetLanguageType[Type](tpe))

    implicit def canReifyInMethodBody[T]: Understands[MethodBodyContext, Reify[T, Expression]]
    def reify[T](tpe: TypeRep.OfHostType[T], value: T): Generator[MethodBodyContext, Expression] =
      AnyParadigm.capabilitiy(Reify[T, Expression](tpe, value))

    implicit val canResolveImportInMethod: Understands[MethodBodyContext, ResolveImport[Import, Type]]
    def resolveImport(tpe: Type): Generator[MethodBodyContext, Option[Import]] =
      AnyParadigm.capabilitiy(ResolveImport[Import, Type](tpe))

    implicit val canApplyInMethodBody: Understands[MethodBodyContext, Apply[Expression, Expression, Expression]]
    def apply(method: Expression, arguments: Seq[Expression]): Generator[MethodBodyContext, Expression] =
      AnyParadigm.capabilitiy(Apply[Expression, Expression, Expression](method, arguments))

    implicit val canGetArgumentsInMethodBody: Understands[MethodBodyContext, GetArguments[Type, Expression]]
    def getArguments(): Generator[MethodBodyContext, Seq[(String, Type, Expression)]] =
      AnyParadigm.capabilitiy(GetArguments[Type, Expression]())
  }
  val methodBodyCapabilities: MethodBodyCapabilities

  trait TestCapabilities {
    implicit val canAddTestCaseInTest: Understands[TestContext, AddTestCase[MethodBodyContext, Expression]]
    def addTestCase(name: String, code: Generator[MethodBodyContext, Seq[Expression]]): Generator[TestContext, Unit] =
      AnyParadigm.capabilitiy(AddTestCase(name, code))
  }
  val testCapabilities: TestCapabilities

  def runGenerator(generator: Generator[ProjectContext, Unit]): Seq[FileWithPath]
}

object AnyParadigm {
  type WithSyntax[S <: AbstractSyntax] = AnyParadigm { val syntax: S }

  def capabilitiy[Ctxt, R, Cmd <: Command.WithResult[R]]
    (cmd: Cmd)
    (implicit interp: Understands[Ctxt, Cmd]): Generator[Ctxt, R] = {
    cmd.interpret[Ctxt, Cmd](interp)
  }

  object syntax {
    def forEach[T, Ctxt, R](xs: Seq[T])(run: T => Generator[Ctxt, R]): Generator[Ctxt, List[R]] =
      xs.toList.map(run).sequence
  }
}