package org.combinators.ep.language.java

import com.github.javaparser.ast.ImportDeclaration
import com.github.javaparser.ast.body.{ClassOrInterfaceDeclaration, MethodDeclaration}
import com.github.javaparser.ast.stmt.BlockStmt
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{AbstractSyntax, Command, FileWithPath, Understands}
import org.combinators.ep.generator.paradigm.{AddBlockDefinitions, AddCompilationUnit, AddImport, AddTestSuite, AnyParadigm, Apply, GetArguments, Reify, ResolveImport, SetParameters, SetReturnType, ToTargetLanguageType}

import scala.collection.JavaConverters._

trait Paradigm extends AnyParadigm {
  val syntax: Syntax.default.type = Syntax.default
  import syntax._

  case class ProjectCtxt(units: Seq[CompilationUnit])
  type ProjectContext = ProjectCtxt

  case class CompilationUnitCtxt(unit: CompilationUnit, isTest: Boolean)
  type CompilationUnitContext = CompilationUnitCtxt

  case class TestCtxt(extraImports: Seq[Import], testClass: ClassOrInterfaceDeclaration)
  override type TestContext = TestCtxt

  case class MethodBodyCtxt(extraImports: Seq[Import], method: MethodDeclaration)
  override type MethodBodyContext = MethodBodyCtxt

  val projectContextCapabilities: ProjectContextCapabilities =
    new ProjectContextCapabilities {
      implicit val canAddCompilationUnitInProject: Understands[ProjectCtxt, AddCompilationUnit[CompilationUnitCtxt]] =
        new Understands[ProjectCtxt, AddCompilationUnit[CompilationUnitCtxt]] {
          def perform(
              context: ProjectCtxt,
              command: AddCompilationUnit[CompilationUnitCtxt]): (ProjectCtxt, Unit) = {
            val (uc, _) =
              Command.runGenerator(command.unit, CompilationUnitCtxt(new com.github.javaparser.ast.CompilationUnit(), false))
            (context.copy(units = context.units :+ uc.unit), ())
          }
        }
    }

  val compilationUnitCapabilities: CompilationUnitCapabilities =
    new CompilationUnitCapabilities {
      implicit val canAddImportInCompilationUnit: Understands[CompilationUnitCtxt, AddImport[ImportDeclaration]] =
        new Understands[CompilationUnitCtxt, AddImport[ImportDeclaration]] {
          def perform(
              context: CompilationUnitCtxt,
              command: AddImport[ImportDeclaration]
            ): (CompilationUnitContext, Unit) = {
            val newUnit = context.unit.clone()
            if (!newUnit.getImports.contains(command.imp)) {
              newUnit.addImport(command.imp)
            }
            (context.copy(unit = newUnit), ())
          }
        }
      implicit val canAddTestSuiteInCompilationUnit: Understands[CompilationUnitCtxt, AddTestSuite[TestCtxt]] =
        new Understands[CompilationUnitCtxt, AddTestSuite[TestCtxt]] {
          def perform(
            context: CompilationUnitCtxt,
            command: AddTestSuite[TestCtxt]
          ): (CompilationUnitContext, Unit) = {
            val newUnit = context.unit.clone
            val (testRes, _) = Command.runGenerator(command.suite, TestCtxt(Seq.empty, new ClassOrInterfaceDeclaration()))
            testRes.extraImports.foreach { imp =>
              if (!newUnit.getImports.contains(imp)) {
                newUnit.addImport(imp)
              }
            }
            testRes.testClass.setName(command.name)
            newUnit.addType(testRes.testClass)
            (context.copy(unit = newUnit, isTest = true), ())
          }
        }
    }
  val methodBodyCapabilities: MethodBodyCapabilities =
    new MethodBodyCapabilities {
      implicit val canAddImportInMethodBody: Understands[MethodBodyCtxt, AddImport[ImportDeclaration]] =
        new Understands[MethodBodyCtxt, AddImport[ImportDeclaration]] {
          def perform(
            context: MethodBodyCtxt,
            command: AddImport[ImportDeclaration]
          ): (MethodBodyCtxt, Unit) =
            (context.copy(extraImports = (context.extraImports :+ command.imp).distinct), ())
        }
      implicit val canAddBlockDefinitionsInMethodBody: Understands[MethodBodyCtxt, AddBlockDefinitions[Statement]] =
        new Understands[MethodBodyCtxt, AddBlockDefinitions[Statement]] {
          def perform(
            context: MethodBodyCtxt,
            command: AddBlockDefinitions[Statement]
          ): (MethodBodyCtxt, Unit) = {
            val newMethod = context.method.clone()
            val body = newMethod.getBody.orElseGet(() => new BlockStmt())
            command.definitions.foreach(stmt => body.addStatement(stmt))
            newMethod.setBody(body)
            (context.copy(method = newMethod), ())
          }
        }

      implicit val canSetReturnTypeInMethodBody: Understands[MethodBodyCtxt, SetReturnType[Type]] =
        new Understands[MethodBodyCtxt, SetReturnType[Type]] {
          def perform(
            context: MethodBodyCtxt,
            command: SetReturnType[Type]
          ): (MethodBodyCtxt, Unit) = {
            val newMethod = context.method.clone()
            newMethod.setType(command.tpe)
            (context.copy(method = newMethod), ())
          }
        }

      implicit val canSetParametersInMethodBody: Understands[MethodBodyCtxt, SetParameters[Type]] =
        new Understands[MethodBodyCtxt, SetParameters[Type]] {
          def perform(
            context: MethodBodyCtxt,
            command: SetParameters[Type]
          ): (MethodBodyCtxt, Unit) = {
            val newMethod = context.method.clone()
            newMethod.getParameters.clear()
            command.params.foreach { case (name, tpe) =>
              newMethod.addParameter(tpe, name)
            }
            (context.copy(method = newMethod), ())
          }
        }
      implicit val canTransformTypeInMethodBody: Understands[MethodBodyCtxt, ToTargetLanguageType[MethodBodyContext, Type]] = ???

      implicit def canReifyInMethodBody[T]: Understands[MethodBodyCtxt, Reify[T, Expression]] = ???
      implicit val canResolveImportInMethod: Understands[MethodBodyCtxt, ResolveImport[ImportDeclaration, Type]] = ???
      implicit val canApplyInMethodBody: Understands[MethodBodyCtxt, Apply[Expression, Expression, Expression]] = ???
      implicit val canGetArgumentsInMethodBody: Understands[MethodBodyCtxt, GetArguments[Type, Expression]] = ???
    }
  val testCapabilities: TestCapabilities = ???
  override def runGenerator(generator: Generator[ProjectContext, Unit]): Seq[FileWithPath] = ???
}
