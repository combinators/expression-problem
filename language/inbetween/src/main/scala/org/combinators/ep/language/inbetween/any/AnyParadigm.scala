package org.combinators.ep.language.inbetween.any

/*DI:LI:AI*/

import org.combinators.cogen.TypeRep
import org.combinators.cogen.paradigm.{AddBlockDefinitions, AddCompilationUnit, AddCustomFile, AddImport, AddMethod, AddTestCase, AddTestSuite, AddTypeLookup, Apply, Debug, FreshName, GetArguments, OutputToConsole, Reify, ResolveImport, SetParameters, SetReturnType, ToTargetLanguageType, AnyParadigm as AP}
import org.combinators.cogen.Command.Generator
import org.combinators.cogen.{Command, FileWithPath, Understands}

trait AnyParadigm[A, S](val ast: AnyAST & A, val syntax: AbstractSyntax.AbstractSyntax[ast.type] & S) extends AP {
  import ast.factory
  import ast.any.*
  val _runGenerator: Generator[ast.any.Project, Unit] => Seq[FileWithPath]
  type ProjectContext = Project
  type CompilationUnitContext = CompilationUnit
  type TestContext = TestSuite
  type MethodBodyContext = Method
  val projectCapabilities: ProjectCapabilities = new ProjectCapabilities {
    implicit val canDebugInProject: Understands[ProjectContext, Debug] = new Understands[ProjectContext, Debug] {
      def perform(context: ProjectContext, command: Debug): (ProjectContext, Unit) = {
        println(command.tag)
        (context, ())
      }
    }
    implicit val canAddCompilationUnitInProject: Understands[ProjectContext, AddCompilationUnit[Name, CompilationUnitContext]] = new Understands[ProjectContext, AddCompilationUnit[Name, CompilationUnitContext]] {
      def perform(context: ProjectContext, command: AddCompilationUnit[Name, CompilationUnitContext]): (ProjectContext, Unit) = {
        val emptyUnit = factory.compilationUnit(command.name, Seq.empty, Seq.empty).initializeInProject(context)
        val (generatedUnit, ()) = Command.runGenerator(command.unit, emptyUnit)
        (context.copy(compilationUnits = context.compilationUnits + generatedUnit), ())
      }
    }
    implicit val canAddTypeLookupForMethodsInProject: Understands[ProjectContext, AddTypeLookup[Method, Type]] = new Understands[ProjectContext, AddTypeLookup[Method, Type]] {
      def perform(context: Project, command: AddTypeLookup[Method, Type]): (Project, Unit) = {
        (context.addTypeLookupsForMethods((tpeRep: TypeRep) => if (tpeRep == command.tpe) Some(command.lookup) else None), ())
      }
    }
    implicit val canAddCustomFile: Understands[ProjectContext, AddCustomFile] = new Understands[ProjectContext, AddCustomFile] {
      def perform(context: ast.any.Project, command: AddCustomFile): (ast.any.Project, Unit) = {
        (context.copy(customFiles = context.customFiles :+ command.file), ())
      }
    }
  }
  val compilationUnitCapabilities: CompilationUnitCapabilities = new CompilationUnitCapabilities {
    implicit val canDebugInCompilationUnit: Understands[CompilationUnit, Debug] = new Understands[CompilationUnitContext, Debug] {
      def perform(context: CompilationUnitContext, command: Debug): (CompilationUnitContext, Unit) = {
        println(command.tag)
        (context, ())
      }
    }
    implicit val canAddImportInCompilationUnit: Understands[CompilationUnit, AddImport[Import]] = new Understands[CompilationUnit, AddImport[Import]] {
      def perform(context: CompilationUnit, command: AddImport[Import]): (CompilationUnit, Unit) = {
        (context.copy(imports = context.imports :+ command.imp), ())
      }
    }
    implicit val canAddTestSuiteInCompilationUnit: Understands[CompilationUnit, AddTestSuite[Name, TestContext]] = new Understands[CompilationUnit, AddTestSuite[Name, TestContext]] {
      def perform(context: CompilationUnit, command: AddTestSuite[Name, TestContext]): (CompilationUnit, Unit) = {
        val emptyTestSuite = factory.testSuite(command.name, Seq.empty).initializeInCompilationUnit(context)
        val (result, ()) = Command.runGenerator(command.suite, emptyTestSuite)
        (context.copy(tests = context.tests :+ result), ())
      }
    }
    implicit val canGetFreshNameInCompilationUnit: Understands[CompilationUnit, FreshName[Name]] = new Understands[CompilationUnit, FreshName[Name]] {
      def perform(context: CompilationUnit, command: FreshName[Name]): (CompilationUnit, Name) = {
        (context, context.getFreshName(command.basedOn))
      }
    }
  }

  val methodBodyCapabilities: MethodBodyCapabilities = new MethodBodyCapabilities {
    implicit val canDebugInMethodBody: Understands[MethodBodyContext, Debug] = new Understands[MethodBodyContext, Debug] {
      def perform(context: MethodBodyContext, command: Debug): (MethodBodyContext, Unit) = {
        (context, ())
      }
    }
    implicit val canOutputToConsole: Understands[MethodBodyContext, OutputToConsole[syntax.Expression]] = new Understands[MethodBodyContext, OutputToConsole[syntax.Expression]] {
      def perform(context: MethodBodyContext, command: OutputToConsole[syntax.Expression]): (MethodBodyContext, Unit) = {
        (context, ())
      }
    }

    implicit val canAddImportInMethodBody: Understands[MethodBodyContext, AddImport[syntax.Import]] = new Understands[MethodBodyContext, AddImport[syntax.Import]] {
      def perform(context: MethodBodyContext, command: AddImport[syntax.Import]): (MethodBodyContext, Unit) = {
        (context.copy(imports = context.imports + command.imp), ())
      }
    }

    implicit val canAddBlockDefinitionsInMethodBody: Understands[MethodBodyContext, AddBlockDefinitions[syntax.Statement]] = new Understands[MethodBodyContext, AddBlockDefinitions[syntax.Statement]] {
      def perform(context: Method, command: AddBlockDefinitions[syntax.Statement]): (Method, Unit) = {
        val stmts: Seq[Statement] = command.definitions
        (context.copy(statements = context.statements ++ stmts), ())
      }
    }
    implicit val canSetReturnTypeInMethodBody: Understands[MethodBodyContext, SetReturnType[syntax.Type]] = new Understands[MethodBodyContext, SetReturnType[syntax.Type]] {
      def perform(context: Method, command: SetReturnType[Type]): (Method, Unit) = {
        (context.copy(returnType = Some(command.tpe)), ())
      }
    }
    implicit val canSetParametersInMethodBody: Understands[MethodBodyContext, SetParameters[syntax.Name, syntax.Type]] = new Understands[MethodBodyContext, SetParameters[syntax.Name, syntax.Type]] {
      def perform(context: Method, command: SetParameters[Name, Type]): (Method, Unit) = {
        (context.copy(parameters = command.params), ())
      }
    }
    implicit val canTransformTypeInMethodBody: Understands[MethodBodyContext, ToTargetLanguageType[syntax.Type]] = new Understands[MethodBodyContext, ToTargetLanguageType[syntax.Type]] {
      def perform(context: Method, command: ToTargetLanguageType[Type]): (Method, Type) = {
        Command.runGenerator(context.toTargetLanguageType(command.tpe), context)
      }
    }
    implicit def canReifyInMethodBody[T]: Understands[MethodBodyContext, Reify[T, syntax.Expression]] = new Understands[MethodBodyContext, Reify[T, syntax.Expression]] {
      def perform(context: Method, command: Reify[T, Expression]): (Method, Expression) = {
        (context, context.reify(command.tpe, command.value))
      }
    }
    implicit val canResolveImportInMethod: Understands[MethodBodyContext, ResolveImport[syntax.Import, syntax.Type]] = new Understands[MethodBodyContext, ResolveImport[syntax.Import, syntax.Type]] {
      def perform(context: Method, command: ResolveImport[Import, Type]): (Method, Option[Import]) = {
        (context, context.resolveImport(command.forElem).headOption) // TODO: Change code generator to Seq[Import]
      }
    }
    implicit val canApplyInMethodBody: Understands[MethodBodyContext, Apply[syntax.Expression, syntax.Expression, syntax.Expression]] = new Understands[MethodBodyContext, Apply[syntax.Expression, syntax.Expression, syntax.Expression]] {
      def perform(context: Method, command: Apply[Expression, Expression, Expression]): (Method, Expression) = {
        (context, factory.applyExpression(command.functional, command.arguments))
      }
    }
    implicit val canGetArgumentsInMethodBody: Understands[MethodBodyContext, GetArguments[syntax.Type, syntax.Name, syntax.Expression]] = new Understands[MethodBodyContext, GetArguments[syntax.Type, syntax.Name, syntax.Expression]] {
      def perform(context: Method, command: GetArguments[Type, Name, Expression]): (Method, Seq[(Name, Type, Expression)]) = {
        val args = context.parameters.map(param => (param._1, param._2, factory.argumentExpression(param._1)))
        (context, args)
      }
    }
    implicit val canGetFreshNameInMethodBody: Understands[MethodBodyContext, FreshName[syntax.Name]] = new Understands[MethodBodyContext, FreshName[syntax.Name]] {
      def perform(context: Method, command: FreshName[Name]): (Method, Name) = {
        (context, context.getFreshName(command.basedOn))
      }
    }
  }
  val testCapabilities: TestCapabilities = new TestCapabilities {
    implicit val canDebugInTest: Understands[TestContext, Debug] = new Understands[TestContext, Debug] {
      def perform(context: TestContext, command: Debug): (TestContext, Unit) = {
        println(command.tag)
        (context, ())
      }
    }

    // Seem to be missing 'canAddBlockDefinitionsInTest

    implicit val canAddTestCaseInTest: Understands[TestContext, AddTestCase[Method, Name, Expression]] = new Understands[TestContext, AddTestCase[Method, Name, Expression]] {
      def perform(context: TestContext, command: AddTestCase[Method, Name, Expression]): (TestContext, Unit) = {
        val emptyMethod = factory.method(
          name = command.name,
          typeLookupMap = context.methodTypeLookupMap
        )
        val (sample, result) = Command.runGenerator(command.code, emptyMethod)

        // break up into no more than 25 at a time. Done because code coverage instrumentation causes the
        // underlying methods to exceed their maximum size.
        val groups = result.sliding(25,25)

        val blocks = groups.map(g => {
          val emptyMethod = factory.method(
            name = sample.getFreshName(command.name),
            typeLookupMap = context.methodTypeLookupMap
          )
          val (generatedMethod, _) = Command.runGenerator(command.code, emptyMethod)
          generatedMethod.addTestExpressions(g)
        })
        (context.copy(tests = context.tests ++ blocks), ())
      }
    }
  }
  def runGenerator(generator: Generator[Project, Unit]): Seq[FileWithPath] = _runGenerator(generator)
}
object AnyParadigm {
  type WithAST[AST <: AnyAST] = AnyParadigm[AST, ? <: AbstractSyntax.AbstractSyntax[AST]] {}
  
  type WithSyntax[AST <: AnyAST, Syntax <: AbstractSyntax.AbstractSyntax[AST]] = AnyParadigm[AST, Syntax] {}

  def apply[AST <: AnyAST, Syntax <: AbstractSyntax.AbstractSyntax[AST]]
    (_ast: AST,
     __runGenerator: Generator[_ast.any.Project, Unit] => Seq[FileWithPath],
     _syntax: Syntax & AbstractSyntax.AbstractSyntax[_ast.type]
    ): WithSyntax[_ast.type, _syntax.type] = new AnyParadigm[_ast.type, _syntax.type](_ast, _syntax) {
      override val _runGenerator: __runGenerator.type = __runGenerator
  }
}
