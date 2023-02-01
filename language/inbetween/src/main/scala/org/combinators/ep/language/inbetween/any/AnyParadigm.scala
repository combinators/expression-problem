package org.combinators.ep.language.inbetween.any

import org.combinators.ep.generator
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{Command, FileWithPath, Understands}
import org.combinators.ep.generator.paradigm.{AddBlockDefinitions, AddCompilationUnit, AddImport, AddTestCase, AddTestSuite, AddTypeLookup, Apply, Debug, FreshName, GetArguments, OutputToConsole, Reify, ResolveImport, SetParameters, SetReturnType, ToTargetLanguageType, AnyParadigm => AP}

class AnyParadigm[FT <: FinalTypes, FatoryType <: Factory[FT]](val factory: FatoryType, _runGenerator: Generator[Project[FT], Unit] => Seq[FileWithPath]) extends AP {
  override val syntax: AbstractSyntax[FT] = new AbstractSyntax[FT]
  type ProjectContext = Project[FT]
  type CompilationUnitContext = CompilationUnit[FT]
  type TestContext = Unit // TODO
  type MethodBodyContext = Method[FT]
  val projectCapabilities: ProjectCapabilities = new ProjectCapabilities {
    implicit val canDebugInProject: Understands[ProjectContext, Debug] = new Understands[ProjectContext, Debug] {
      def perform(context: ProjectContext, command: Debug): (ProjectContext, Unit) = {
        println(command.tag)
        (context, ())
      }
    }
    implicit val canAddCompilationUnitInProject: Understands[ProjectContext, AddCompilationUnit[Name[FT], CompilationUnitContext]] = new Understands[ProjectContext, AddCompilationUnit[Name[FT], CompilationUnitContext]] {
      def perform(context: ProjectContext, command: AddCompilationUnit[Name[FT], CompilationUnitContext]): (ProjectContext, Unit) = {
        val emptyUnit = factory.compilationUnit(command.name, Seq.empty)
        val (generatedUnit, ()) = Command.runGenerator(command.unit, emptyUnit)
        (context.copy(compilationUnits = context.compilationUnits + generatedUnit), ())
      }
    }
    implicit val canAddTypeLookupForMethodsInProject: Understands[ProjectContext, AddTypeLookup[Method[FT], Type[FT]]] = new Understands[ProjectContext, AddTypeLookup[Method[FT], Type[FT]]] {
      def perform(context: Project[FT], command: AddTypeLookup[Method[FT], Type[FT]]): (Project[FT], Unit) = {

        (context.addTypeLookupForMethods(command.tpe, command.lookup), ())
      }
    }
  }
  val compilationUnitCapabilities: CompilationUnitCapabilities = new CompilationUnitCapabilities {
    implicit val canDebugInCompilationUnit: Understands[CompilationUnit[FT], Debug] = new Understands[CompilationUnitContext, Debug] {
      def perform(context: CompilationUnitContext, command: Debug): (CompilationUnitContext, Unit) = {
        println(command.tag)
        (context, ())
      }
    }
    implicit val canAddImportInCompilationUnit: Understands[CompilationUnit[FT], AddImport[Import[FT]]] = new Understands[CompilationUnit[FT], AddImport[Import[FT]]] {
      def perform(context: CompilationUnit[FT], command: AddImport[Import[FT]]): (CompilationUnit[FT], Unit) = {
        (context.copy(imports = context.imports :+ command.imp), ())
      }
    }
    implicit val canAddTestSuiteInCompilationUnit: Understands[CompilationUnit[FT], AddTestSuite[Name[FT], TestContext]] = new Understands[CompilationUnit[FT], AddTestSuite[Name[FT], TestContext]] {
      def perform(context: CompilationUnit[FT], command: AddTestSuite[Name[FT], TestContext]): (CompilationUnit[FT], Unit) = {
        (context, ()) // TODO
      }
    }
    implicit val canGetFreshNameInCompilationUnit: Understands[CompilationUnit[FT], FreshName[Name[FT]]] = new Understands[CompilationUnit[FT], FreshName[Name[FT]]] {
      def perform(context: CompilationUnit[FT], command: FreshName[Name[FT]]): (CompilationUnit[FT], Name[FT]) = {
        (context, context.getFreshName(command.basedOn))
      }
    }
  }

  val methodBodyCapabilities: MethodBodyCapabilities = new MethodBodyCapabilities {
    implicit val canDebugInMethodBody: Understands[MethodBodyContext, Debug] = new Understands[MethodBodyContext, Debug] {
      def perform(context: MethodBodyContext, command: Debug): (MethodBodyContext, Unit) = {
        println(command.tag)
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
      def perform(context: Method[FT], command: AddBlockDefinitions[syntax.Statement]): (Method[FT], Unit) = {
        val stmts: Seq[Statement[FT]] = command.definitions
        (context.copy(statements = context.statements ++ stmts), ())
      }
    }
    implicit val canSetReturnTypeInMethodBody: Understands[MethodBodyContext, SetReturnType[syntax.Type]] = new Understands[MethodBodyContext, SetReturnType[syntax.Type]] {
      def perform(context: Method[FT], command: SetReturnType[Type[FT]]): (Method[FT], Unit) = {
        (context.copy(returnType = Some(command.tpe)), ())
      }
    }
    implicit val canSetParametersInMethodBody: Understands[MethodBodyContext, SetParameters[syntax.Name, syntax.Type]] = new Understands[MethodBodyContext, SetParameters[syntax.Name, syntax.Type]] {
      def perform(context: Method[FT], command: SetParameters[Name[FT], Type[FT]]): (Method[FT], Unit) = {
        (context.copy(parameters = command.params), ())
      }
    }
    implicit val canTransformTypeInMethodBody: Understands[MethodBodyContext, ToTargetLanguageType[syntax.Type]] = new Understands[MethodBodyContext, ToTargetLanguageType[syntax.Type]] {
      def perform(context: Method[FT], command: ToTargetLanguageType[Type[FT]]): (Method[FT], Type[FT]) = {
        (context, context.toTargetLanguageType(command.tpe))
      }
    }
    implicit def canReifyInMethodBody[T]: Understands[MethodBodyContext, Reify[T, syntax.Expression]] = new Understands[MethodBodyContext, Reify[T, syntax.Expression]] {
      def perform(context: Method[FT], command: Reify[T, Expression[FT]]): (Method[FT], Expression[FT]) = {
        (context, context.reify(command.tpe, command.value))
      }
    }
    implicit val canResolveImportInMethod: Understands[MethodBodyContext, ResolveImport[syntax.Import, syntax.Type]] = new Understands[MethodBodyContext, ResolveImport[syntax.Import, syntax.Type]] {
      def perform(context: Method[FT], command: ResolveImport[Import[FT], Type[FT]]): (Method[FT], Option[Import[FT]]) = {
        (context, context.resolveImport(command.forElem))
      }
    }
    implicit val canApplyInMethodBody: Understands[MethodBodyContext, Apply[syntax.Expression, syntax.Expression, syntax.Expression]] = new Understands[MethodBodyContext, Apply[syntax.Expression, syntax.Expression, syntax.Expression]] {
      def perform(context: Method[FT], command: Apply[Expression[FT], Expression[FT], Expression[FT]]): (Method[FT], Expression[FT]) = {
        (context, factory.applyExpression(command.functional, command.arguments))
      }
    }
    implicit val canGetArgumentsInMethodBody: Understands[MethodBodyContext, GetArguments[syntax.Type, syntax.Name, syntax.Expression]] = new Understands[MethodBodyContext, GetArguments[syntax.Type, syntax.Name, syntax.Expression]] {
      def perform(context: Method[FT], command: GetArguments[Type[FT], Name[FT], Expression[FT]]): (Method[FT], Seq[(Name[FT], Type[FT], Expression[FT])]) = {
        val args = context.parameters.map(param => (param._1, param._2, factory.argumentExpression(param._1)))
        (context, args)
      }
    }
    implicit val canGetFreshNameInMethodBody: Understands[MethodBodyContext, FreshName[syntax.Name]] = new Understands[MethodBodyContext, FreshName[syntax.Name]] {
      def perform(context: Method[FT], command: FreshName[Name[FT]]): (Method[FT], Name[FT]) = {
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
    implicit val canAddTestCaseInTest: Understands[TestContext, AddTestCase[Method[FT], Name[FT], Expression[FT]]] = new Understands[TestContext, AddTestCase[Method[FT], Name[FT], Expression[FT]]] {
      def perform(context: TestContext, command: AddTestCase[Method[FT], Name[FT], Expression[FT]]): (TestContext, Unit) = {
        (context, ()) // TODO
      }
    }
  }
  def runGenerator(generator: Generator[Project[FT], Unit]): Seq[FileWithPath] = _runGenerator(generator)
}
