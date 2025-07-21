package org.combinators.cogen.paradigm

import org.combinators.cogen.{AbstractSyntax, Command, FileWithPath, Understands, TypeRep}
import Command._
import cats.implicits._    // needed for .sequence

/** Adds a compilation unit with sequence of names for hierarchy, if needed */
case class AddCompilationUnit[Name, CompilationUnitContext](unit: Generator[CompilationUnitContext, Unit], name: Seq[Name])
    extends Command {
  type Result = Unit
}

/** Adds the given import. Static Import = 1. */
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
case class AddMethod[MethodBodyContext, Name, Expression](
   name: Name,
   spec: Generator[MethodBodyContext, Expression],
   isPublic: Boolean = true,
   isOverride: Boolean = false
  ) extends Command {
  type Result = Unit
}

case class SetReturnType[Type](tpe: Type) extends Command {
  type Result = Unit
}

case class SetParameters[Name, Type](params: Seq[(Name, Type)]) extends Command {
  type Result = Unit
}

case class GetArguments[Type, Name, Expression]() extends Command {
  type Result = Seq[(Name, Type, Expression)]
}

case class AddTestSuite[Name, TestContext](name: Name, suite: Generator[TestContext, Unit]) extends Command {
  type Result = Unit
}
case class AddTestCase[MethodBodyContext, Name, Expression](code: Generator[MethodBodyContext, Seq[Expression]], name: Name) extends Command {
  type Result = Unit
}

case class AddImplementedTestCase[Type](interface: Type) extends Command {
  type Result = Unit
}

case class Apply[F, S, R](functional: F, arguments: Seq[S]) extends Command {
  type Result = R
}

case class ResolveImport[Import, T](forElem: T) extends Command {
  type Result = Option[Import]
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

case class FreshName[Name](basedOn: Name) extends Command {
  type Result = Name
}

/** Ability for output of partial generated artifacts. Tag debug statements with tag. */
case class Debug(tag: String) extends Command {
  type Result = Unit
}

/** Can output an expression to the console. */
case class OutputToConsole[Expression](expr: Expression) extends Command {
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
  trait ProjectCapabilities {
    implicit val canDebugInProject: Understands[ProjectContext, Debug]
    def debug(tag:String = ""): Generator[ProjectContext, Unit] =
      AnyParadigm.capability(Debug(tag))

    implicit val canAddCompilationUnitInProject: Understands[ProjectContext, AddCompilationUnit[Name, CompilationUnitContext]]
    def addCompilationUnit(unit: Generator[CompilationUnitContext, Unit], qualifiedName: Name*): Generator[ProjectContext, Unit] =
      AnyParadigm.capability(AddCompilationUnit(unit, qualifiedName))

    implicit val canAddTypeLookupForMethodsInProject: Understands[ProjectContext, AddTypeLookup[MethodBodyContext, Type]]
    def addTypeLookupForMethods(tpe: TypeRep, lookup: Generator[MethodBodyContext, Type]): Generator[ProjectContext, Unit] =
      AnyParadigm.capability(AddTypeLookup[MethodBodyContext, Type](tpe, lookup))
  }
  val projectCapabilities: ProjectCapabilities

  /** Each CompilationUnit may have external import dependencies and associated test cases. */
  trait CompilationUnitCapabilities {
    implicit val canDebugInCompilationUnit: Understands[CompilationUnitContext, Debug]
    def debug(tag:String = ""): Generator[CompilationUnitContext, Unit] =
      AnyParadigm.capability(Debug(tag))

    implicit val canAddImportInCompilationUnit: Understands[CompilationUnitContext, AddImport[Import]]
    def addImport(imp: Import): Generator[CompilationUnitContext, Unit] =
      AnyParadigm.capability(AddImport(imp))

    implicit val canAddTestSuiteInCompilationUnit: Understands[CompilationUnitContext, AddTestSuite[Name, TestContext]]
    def addTestSuite(name: Name, suite: Generator[TestContext, Unit]): Generator[CompilationUnitContext, Unit] =
      AnyParadigm.capability(AddTestSuite[Name, TestContext](name, suite))

    implicit val canGetFreshNameInCompilationUnit: Understands[CompilationUnitContext, FreshName[Name]]
    def freshName(basedOn: Name): Generator[CompilationUnitContext, Name] =
      AnyParadigm.capability(FreshName[Name](basedOn))
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
    implicit val canDebugInMethodBody: Understands[MethodBodyContext, Debug]
    def debug(tag:String = ""): Generator[MethodBodyContext, Unit] =
      AnyParadigm.capability(Debug(tag))

    implicit val canOutputToConsole: Understands[MethodBodyContext, OutputToConsole[Expression]]
    def output(expr:Expression): Generator[MethodBodyContext, Unit] =
      AnyParadigm.capability(OutputToConsole[Expression](expr))
    
    implicit val canAddImportInMethodBody: Understands[MethodBodyContext, AddImport[Import]]
    def addImport(imp: Import): Generator[MethodBodyContext, Unit] =
      AnyParadigm.capability(AddImport(imp))

    implicit val canAddBlockDefinitionsInMethodBody: Understands[MethodBodyContext, AddBlockDefinitions[Statement]]
    def addBlockDefinitions(definitions: Seq[Statement]): Generator[MethodBodyContext, Unit] =
      AnyParadigm.capability(AddBlockDefinitions[Statement](definitions))

    implicit val canSetReturnTypeInMethodBody: Understands[MethodBodyContext, SetReturnType[Type]]
    def setReturnType(tpe: Type): Generator[MethodBodyContext, Unit] =
      AnyParadigm.capability(SetReturnType(tpe))

    implicit val canSetParametersInMethodBody: Understands[MethodBodyContext, SetParameters[Name, Type]]
    def setParameters(params: Seq[(Name, Type)]): Generator[MethodBodyContext, Unit] =
      AnyParadigm.capability(SetParameters(params))

    implicit val canTransformTypeInMethodBody: Understands[MethodBodyContext, ToTargetLanguageType[Type]]
    def toTargetLanguageType(tpe: TypeRep): Generator[MethodBodyContext, Type] =
      AnyParadigm.capability(ToTargetLanguageType[Type](tpe))

    implicit def canReifyInMethodBody[T]: Understands[MethodBodyContext, Reify[T, Expression]]
    def reify[T](tpe: TypeRep.OfHostType[T], value: T): Generator[MethodBodyContext, Expression] =
      AnyParadigm.capability(Reify[T, Expression](tpe, value))

    implicit val canResolveImportInMethod: Understands[MethodBodyContext, ResolveImport[Import, Type]]
    def resolveImport(tpe: Type): Generator[MethodBodyContext, Option[Import]] =
      AnyParadigm.capability(ResolveImport[Import, Type](tpe))

    implicit val canApplyInMethodBody: Understands[MethodBodyContext, Apply[Expression, Expression, Expression]]
    def apply(method: Expression, arguments: Seq[Expression]): Generator[MethodBodyContext, Expression] =
      AnyParadigm.capability(Apply[Expression, Expression, Expression](method, arguments))

    implicit val canGetArgumentsInMethodBody: Understands[MethodBodyContext, GetArguments[Type, Name, Expression]]
    def getArguments(): Generator[MethodBodyContext, Seq[(Name, Type, Expression)]] =
      AnyParadigm.capability(GetArguments[Type, Name, Expression]())

    implicit val canGetFreshNameInMethodBody: Understands[MethodBodyContext, FreshName[Name]]
    def freshName(basedOn: Name): Generator[MethodBodyContext, Name] =
      AnyParadigm.capability(FreshName[Name](basedOn))
  }
  val methodBodyCapabilities: MethodBodyCapabilities

  trait TestCapabilities {
    implicit val canDebugInTest: Understands[TestContext, Debug]
    def debug(tag:String = ""): Generator[TestContext, Unit] =
      AnyParadigm.capability(Debug(tag))

    implicit val canAddTestCaseInTest: Understands[TestContext, AddTestCase[MethodBodyContext, Name, Expression]]
    def addTestCase(code: Generator[MethodBodyContext, Seq[Expression]], name: Name): Generator[TestContext, Unit] =
      AnyParadigm.capability(AddTestCase(code, name))
  }
  val testCapabilities: TestCapabilities

  def runGenerator(generator: Generator[ProjectContext, Unit]): Seq[FileWithPath]
}

object AnyParadigm {
  type WithSyntax[S <: AbstractSyntax] = AnyParadigm { val syntax: S }

  def capability[Ctxt, R, Cmd <: Command.WithResult[R]]
    (cmd: Cmd)
    (implicit interp: Understands[Ctxt, Cmd]): Generator[Ctxt, R] = {
    cmd.interpret[Ctxt, Cmd](interp)
  }

  object syntax {
    def forEach[T, Ctxt, R](xs: Seq[T])(run: T => Generator[Ctxt, R]): Generator[Ctxt, List[R]] =
      xs.toList.map(run).sequence
  }
}