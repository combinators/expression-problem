package org.combinators.ep.language.java

import java.nio.file.Paths
import java.util.UUID

import com.github.javaparser.ast.`type`.{ClassOrInterfaceType, TypeParameter}
import com.github.javaparser.ast.{ImportDeclaration, Modifier, NodeList, PackageDeclaration}
import com.github.javaparser.ast.body.{ClassOrInterfaceDeclaration, ConstructorDeclaration, MethodDeclaration}
import com.github.javaparser.ast.expr.{AssignExpr, BinaryExpr, BooleanLiteralExpr, DoubleLiteralExpr, MethodCallExpr, NameExpr, SimpleName, StringLiteralExpr, TypeExpr, UnaryExpr, VariableDeclarationExpr}
import com.github.javaparser.ast.stmt.{BlockStmt, ExplicitConstructorInvocationStmt, ExpressionStmt, IfStmt, ReturnStmt, WhileStmt}
import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.ObjectOriented.WithBase
import org.combinators.ep.generator.paradigm.ParametricPolymorphism.WithBase
import org.combinators.ep.generator.paradigm.control.{AssignVariable, Imperative, Return, While}
import org.combinators.ep.generator.{Command, FileWithPath, FreshNameProvider, Understands}
import org.combinators.ep.generator.paradigm._
import org.combinators.templating.twirl.Java
import org.combinators.templating.persistable.{BundledResource, JavaPersistable, ResourcePersistable}

import scala.collection.JavaConverters._
import scala.util.Try
import cats.{Apply => _, _}
import cats.implicits._
import cats.data.State
import org.combinators.ep.generator.paradigm.ffi.{Add, And, Arithmetic, Assert, Assertions, Booleans, Div, Equality, False, GetStringLength, Mod, Mult, Not, Or, StringAppend, Strings, Sub, ToString, True}
import org.combinators.ep.language.java.CodeGenerator.BoxLevel
import org.combinators.ep.language.java.Syntax.MangledName


/**
 * Java-specific.
 *
 * These paradigm-specific traits are conceptually different from each other
 */
sealed class CodeGenerator(config: CodeGenerator.Config) { cc =>
  import Syntax.default._

  case class ContextSpecificResolver(
    methodTypeResolution: TypeRep => Generator[MethodBodyCtxt, Type],
    constructorTypeResolution: TypeRep => Generator[CtorCtxt, Type],
    classTypeResolution: TypeRep => Generator[ClassCtxt, Type],
    reificationInConstructor: InstanceRep => Generator[CtorCtxt, Expression],
    reificationInMethod: InstanceRep => Generator[MethodBodyCtxt, Expression],
    importResolution: Type => Option[Import],
    instantiationOverride: (Type, Seq[Expression]) => (Type, Seq[Expression]),
    generatedVariables: Map[String, MangledName]
  )

  case class ProjectCtxt(
    resolver: ContextSpecificResolver,
    units: Seq[com.github.javaparser.ast.CompilationUnit],
    testUnits: Seq[com.github.javaparser.ast.CompilationUnit],
    extraDependencies: Seq[String]
  )
  case class CompilationUnitCtxt(
    resolver: ContextSpecificResolver,
    unit: com.github.javaparser.ast.CompilationUnit,
    isTest: Boolean
  )
  case class ClassCtxt(
    resolver: ContextSpecificResolver,
    cls: ClassOrInterfaceDeclaration,
    extraImports: Seq[Import]
  )
  case class TestCtxt(
    resolver: ContextSpecificResolver,
    extraImports: Seq[Import],
    testClass: ClassOrInterfaceDeclaration
  )
  case class MethodBodyCtxt(
    resolver: ContextSpecificResolver,
    extraImports: Seq[Import],
    method: MethodDeclaration
  )
  case class CtorCtxt(
    resolver: ContextSpecificResolver,
    extraImports: Seq[Import],
    ctor: ConstructorDeclaration
  )
  case class TypeParamCtxt(
    param: TypeParameter
  )



  object paradigm extends AnyParadigm {
    val syntax: Syntax.default.type = Syntax.default
    type ProjectContext = ProjectCtxt
    type CompilationUnitContext = CompilationUnitCtxt
    type TestContext = TestCtxt
    type MethodBodyContext = MethodBodyCtxt


    val projectContextCapabilities: ProjectContextCapabilities =
      new ProjectContextCapabilities {
        implicit val canAddCompilationUnitInProject: Understands[ProjectCtxt, AddCompilationUnit[Name, CompilationUnitCtxt]] =
          new Understands[ProjectCtxt, AddCompilationUnit[Name, CompilationUnitCtxt]] {
            def perform(
              context: ProjectCtxt,
              command: AddCompilationUnit[Name, CompilationUnitCtxt]
            ): (ProjectCtxt, Unit) = {
              val unit = new com.github.javaparser.ast.CompilationUnit()
              unit.setPackageDeclaration(config.targetPackage)
              val (uc, _) =
                Command.runGenerator(
                  command.unit,
                  CompilationUnitCtxt(
                    context.resolver,
                    unit,
                    isTest = false)
                )
              val (newUnits, newTestUnits) =
                if (uc.isTest) {
                  (context.units, context.testUnits :+ uc.unit)
                } else {
                  (context.units :+ uc.unit, context.testUnits)
                }
              (context.copy(resolver = uc.resolver, units = newUnits, testUnits = newTestUnits), ())
            }
          }
        implicit val canAddTypeLookupForMethodsInProject: Understands[ProjectContext, AddTypeLookup[MethodBodyContext, Type]] =
          new Understands[ProjectContext, AddTypeLookup[MethodBodyContext, Type]] {
            def perform(
              context: ProjectContext,
              command: AddTypeLookup[MethodBodyCtxt, Type]
            ): (ProjectContext, Unit) = {
              def newLookup(tpe: TypeRep): Generator[MethodBodyCtxt, Type] =
                if (tpe == command.tpe) {
                  command.lookup
                } else {
                  context.resolver.methodTypeResolution(tpe)
                }
              (context.copy(resolver = context.resolver.copy(methodTypeResolution = newLookup)), ())
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
              val oldUnit = context.unit
              val newUnit =
                if (!oldUnit.getImports.contains(command.imp)) {
                  val nextUnit = oldUnit.clone()
                  nextUnit.addImport(command.imp)
                  nextUnit
                } else oldUnit
              newUnit.getImports.sort((i1, i2) => i1.toString.compareTo(i2.toString))
              (context.copy(unit = newUnit), ())
            }
          }
        implicit val canAddTestSuiteInCompilationUnit: Understands[CompilationUnitCtxt, AddTestSuite[Name, TestCtxt]] =
          new Understands[CompilationUnitCtxt, AddTestSuite[Name, TestCtxt]] {
            def perform(
              context: CompilationUnitCtxt,
              command: AddTestSuite[Name, TestCtxt]
            ): (CompilationUnitContext, Unit) = {
              val clsToAdd = new ClassOrInterfaceDeclaration()
              clsToAdd.setPublic(true)
              val (testRes, _) =
                Command.runGenerator(
                  command.suite,
                  TestCtxt(
                    context.resolver,
                    Seq.empty,
                    clsToAdd))
              val updatedUnit = {
                val oldUnit = context.unit
                val testClass = testRes.testClass
                val nextUnit = oldUnit.clone()
                testRes.extraImports.foreach { imp =>
                  if (!nextUnit.getImports.contains(imp))
                    nextUnit.addImport(imp.clone())
                }
                val classToAdd = testClass.clone()
                classToAdd.setName(command.name.mangled)
                nextUnit.addType(classToAdd)
                nextUnit.getImports.sort((i1, i2) => i1.toString.compareTo(i2.toString))
                nextUnit
              }
              (context.copy(resolver = testRes.resolver, unit = updatedUnit, isTest = true), ())
            }
          }

        implicit val canGetFreshNameInCompilationUnit: Understands[CompilationUnitContext, FreshName[Name]] =
          new Understands[CompilationUnitContext, FreshName[Name]] {
            def perform(context: CompilationUnitContext, command: FreshName[Name]): (CompilationUnitContext, Name) = {
              val freshName = JavaNameProvider.mangle(s"$$$$generatedName_${UUID.randomUUID().toString.replace("-", "_")}$$$$")
              val updatedResolver = context.resolver.copy(
                generatedVariables = context.resolver.generatedVariables + (freshName.toAST.getIdentifier -> command.basedOn)
              )
              (context.copy(resolver = updatedResolver), freshName)
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
            ): (MethodBodyCtxt, Unit) = {
              val extraImports = (context.extraImports :+ command.imp).distinct.map(_.clone())
              (context.copy(extraImports = extraImports), ())
            }
          }
        implicit val canAddBlockDefinitionsInMethodBody: Understands[MethodBodyCtxt, AddBlockDefinitions[Statement]] =
          new Understands[MethodBodyCtxt, AddBlockDefinitions[Statement]] {
            def perform(
              context: MethodBodyCtxt,
              command: AddBlockDefinitions[Statement]
            ): (MethodBodyCtxt, Unit) = {
              val updatedMethod = {
                val oldMethod = context.method
                val stmts = command.definitions
                val nextMethod = oldMethod.clone()
                val body = nextMethod.getBody.orElseGet(() => new BlockStmt())
                stmts.foreach(stmt => body.addStatement(stmt.clone()))
                nextMethod.setBody(body)
                nextMethod
              }
              (context.copy(method = updatedMethod), ())
            }
          }

        implicit val canSetReturnTypeInMethodBody: Understands[MethodBodyCtxt, SetReturnType[Type]] =
          new Understands[MethodBodyCtxt, SetReturnType[Type]] {
            def perform(
              context: MethodBodyCtxt,
              command: SetReturnType[Type]
            ): (MethodBodyCtxt, Unit) = {
              val updatedMethod =  {
                val oldMethod = context.method
                val tpe = command.tpe
                val newMethod = oldMethod.clone()
                newMethod.setType(tpe.clone())
                newMethod
              }
              (context.copy(method = updatedMethod), ())
            }
          }

        implicit val canSetParametersInMethodBody: Understands[MethodBodyCtxt, SetParameters[Name, Type]] =
          new Understands[MethodBodyCtxt, SetParameters[Name, Type]] {
            def perform(
              context: MethodBodyCtxt,
              command: SetParameters[Name, Type]
            ): (MethodBodyCtxt, Unit) = {
              val updatedMethod = {
                val oldMethod = context.method
                val params = command.params
                val newMethod = oldMethod.clone()
                newMethod.getParameters.clear()
                params.foreach { case (paramName, paramTpe) =>
                  newMethod.addParameter(paramTpe.clone(), paramName.mangled)
                }
                newMethod
              }
              (context.copy(method = updatedMethod), ()) // second thing to be returned isn't optional, so make it () is like Unit
            }
          }
        implicit val canTransformTypeInMethodBody: Understands[MethodBodyCtxt, ToTargetLanguageType[Type]] =
          new Understands[MethodBodyContext, ToTargetLanguageType[Type]] {
            def perform(
              context: MethodBodyContext,
              command: ToTargetLanguageType[Type]
            ): (MethodBodyContext, Type) = {
              Command.runGenerator(context.resolver.methodTypeResolution(command.tpe), context)
            }
          }

        implicit def canReifyInMethodBody[T]: Understands[MethodBodyCtxt, Reify[T, Expression]] =
          new Understands[MethodBodyCtxt, Reify[T, Expression]] {
            def perform(
              context: MethodBodyCtxt,
              command: Reify[T, Expression]
            ): (MethodBodyCtxt, Expression) = {
              Command.runGenerator(context.resolver.reificationInMethod(InstanceRep(command.tpe)(command.value)), context)
            }
          }

        implicit val canResolveImportInMethod: Understands[MethodBodyCtxt, ResolveImport[ImportDeclaration, Type]] =
          new Understands[MethodBodyCtxt, ResolveImport[ImportDeclaration, Type]] {
            def perform(
              context: MethodBodyCtxt,
              command: ResolveImport[ImportDeclaration, Type]
            ): (MethodBodyCtxt, Option[ImportDeclaration]) = {
              Try { (context, context.resolver.importResolution(command.forElem)) } getOrElse {
                if (command.forElem.isClassOrInterfaceType) {
                  val importName = command.forElem.asClassOrInterfaceType().getNameAsString
                  val newImport =
                    new ImportDeclaration(
                      new com.github.javaparser.ast.expr.Name(config.targetPackage.getName.clone(), importName),
                      false,
                      false)
                  if (context.extraImports.contains(newImport)) {
                    (context, None)
                  } else {
                    (context, Some(newImport))
                  }
                } else {
                  (context, None)
                }
              }
            }
          }

        implicit val canApplyInMethodBody: Understands[MethodBodyCtxt, Apply[Expression, Expression, Expression]] =
          new Understands[MethodBodyCtxt, Apply[Expression, Expression, Expression]] {
            def perform(
              context: MethodBodyCtxt,
              command: Apply[Expression, Expression, Expression]
            ): (MethodBodyCtxt, Expression) = {
              val resultExp: Expression =
                if (command.functional.isMethodCallExpr) {
                  val res = command.functional.asMethodCallExpr().clone()
                  command.arguments.foreach(arg => res.addArgument(arg))
                  res
                } else {
                  Java(s"${command.functional}${command.arguments.mkString("(", ", ", ")")}").expression()
                }
              (context, resultExp)
            }
          }

        implicit val canGetArgumentsInMethodBody: Understands[MethodBodyCtxt, GetArguments[Type, Name, Expression]] =
          new Understands[MethodBodyCtxt, GetArguments[Type, Name, Expression]] {
            def perform(
              context: MethodBodyCtxt,
              command: GetArguments[Type, Name, Expression]
            ): (MethodBodyCtxt, Seq[(Name, Type, Expression)]) = {
              val params = context.method.getParameters.asScala.map { param =>
                (MangledName.fromAST(param.getName), param.getType, new NameExpr(param.getName))
              }
              (context, params)
            }
          }
        implicit val canGetFreshNameInMethodBody: Understands[MethodBodyContext, FreshName[Name]] =
          new Understands[MethodBodyContext, FreshName[Name]] {
            def perform(context: MethodBodyContext, command: FreshName[MangledName]): (MethodBodyContext, MangledName) = {
              val freshName = JavaNameProvider.mangle(s"$$$$generatedName_${UUID.randomUUID().toString.replace("-", "_")}$$$$")
              val updatedResolver = context.resolver.copy(
                generatedVariables = context.resolver.generatedVariables + (freshName.toAST.getIdentifier -> command.basedOn)
              )
              (context.copy(resolver = updatedResolver), freshName)
            }
          }
      }
      val testCapabilities: TestCapabilities =
        new TestCapabilities {
          implicit val canAddTestCaseInTest: Understands[TestContext, AddTestCase[MethodBodyContext, Name, Expression]] =
            new Understands[TestContext, AddTestCase[MethodBodyContext, Name, Expression]] {
              def perform(
                context: TestContext,
                command: AddTestCase[MethodBodyContext, Name, Expression]
              ): (TestContext, Unit) = {
                val gen: Generator[MethodBodyCtxt, Unit] = {
                  import methodBodyCapabilities._
                  for {
                    assertions <- command.code
                    _ <- addBlockDefinitions(assertions.map(exp => new ExpressionStmt(exp.clone())))
                  } yield ()
                }
                val testMethod = new MethodDeclaration()
                testMethod.setModifiers(Modifier.publicModifier().getKeyword)
                testMethod.setType(new com.github.javaparser.ast.`type`.VoidType())
                testMethod.setName(JavaNameProvider.addPrefix("test", command.name).toAST)
                testMethod.addMarkerAnnotation("Test")
                val testImport = new ImportDeclaration("org.junit.Test", false, false)
                val (resultingContext, _) =
                  Command.runGenerator(
                    gen,
                    MethodBodyCtxt(
                      context.resolver,
                      (testImport +: context.extraImports).distinct.map(_.clone),
                      testMethod)
                  )
                val newClass = context.testClass.clone()
                newClass.addMember(resultingContext.method.clone)
                (context.copy(resolver = resultingContext.resolver, testClass = newClass, extraImports = resultingContext.extraImports), ())
              }
            }
        }
      override def runGenerator(generator: Generator[ProjectContext, Unit]): Seq[FileWithPath] = {
        val (finalContext, _) =
          Command.runGenerator(generator,
            ProjectCtxt(
              ContextSpecificResolver(
                methodTypeResolution = _ => ???,
                constructorTypeResolution = _ => ???,
                classTypeResolution = _ => ???,
                reificationInConstructor = _ => ???,
                reificationInMethod = _ => ???,
                importResolution = _ => ???,
                instantiationOverride = (tpe, args) => (tpe, args),
                generatedVariables = Map.empty
              ),
              units = Seq.empty,
              testUnits = Seq.empty,
              extraDependencies = Seq.empty
            )
          )
        val nameEntry = config.projectName.map(n => s"""name := "${n}"""").getOrElse("")
        val junitDep = """"com.novocode" % "junit-interface" % "0.11" % "test""""
        val deps = (junitDep +: finalContext.extraDependencies).mkString("Seq(\n    ", ",\n    ", "\n  )")
        val buildFile =
          s"""
             |$nameEntry
             |crossPaths := false
             |autoScalaLibrary := false
             |libraryDependencies ++= $deps
           """.stripMargin
        val cleanedUnits = FreshNameCleanup.cleaned(finalContext.resolver.generatedVariables, finalContext.units: _*)
        val cleanedTestUnits = FreshNameCleanup.cleaned(finalContext.resolver.generatedVariables, finalContext.testUnits: _*)
        val javaFiles = cleanedUnits.map { unit =>
          FileWithPath(
            JavaPersistable.compilationUnitInstance.rawText(unit),
            JavaPersistable.compilationUnitInstance.fullPath(Paths.get("."), unit)
          )
        }
        val javaTestFiles = cleanedTestUnits.map { unit =>
          val javaPath =
            Paths.get("src", "main")
              .relativize(JavaPersistable.compilationUnitInstance.fullPath(Paths.get("."), unit))
          val testPath =
            Paths.get("src", "test").resolve(javaPath)
          FileWithPath(
            JavaPersistable.compilationUnitInstance.rawText(unit),
            testPath
          )
        }
        val gitIgnore = BundledResource(".gitignore", Paths.get(".gitignore"), this.getClass)
        FileWithPath(
          ResourcePersistable.bundledResourceInstance.rawText(gitIgnore),
          ResourcePersistable.bundledResourceInstance.path(gitIgnore)) +:
          FileWithPath(buildFile, Paths.get("build.sbt")) +:
          (javaFiles ++ javaTestFiles)
      }
    }

  object ooParadigm extends ObjectOriented {
      val base: paradigm.type = paradigm
      type ClassContext = ClassCtxt
      type ConstructorContext = CtorCtxt
      val compilationUnitCapabilities: CompilationUnitCapabilities =
        new CompilationUnitCapabilities {
          implicit val canAddClassInCompilationUnit: Understands[base.CompilationUnitContext, AddClass[ClassContext, Name]] =
            new Understands[base.CompilationUnitContext, AddClass[ClassContext, Name]] {
              def perform(
                context: base.CompilationUnitContext,
                command: AddClass[ClassContext, Name]
              ): (base.CompilationUnitContext, Unit) = {
                val clsToAdd = new ClassOrInterfaceDeclaration()
                clsToAdd.setName(command.name.toAST)
                clsToAdd.setPublic(true)
                val (resultCtxt, _) =
                  Command.runGenerator(
                    command.cls,
                    ClassCtxt(context.resolver, clsToAdd, context.unit.getImports().asScala)
                  )
                val newUnit = context.unit.clone()
                newUnit.addType(resultCtxt.cls)
                resultCtxt.extraImports.foreach { imp =>
                  if (!newUnit.getImports.contains(imp) && imp.getName.getIdentifier != command.name.toAST.getIdentifier) {
                    newUnit.addImport(imp.clone())
                  }
                }
                newUnit.getImports.sort((i1, i2) => i1.toString.compareTo(i2.toString))
                (context.copy(resolver = resultCtxt.resolver, unit = newUnit), ())
              }
            }
        }

      val classCapabilities: ClassCapabilities =
        new ClassCapabilities {
          import base.syntax._
          implicit val canAddParentInClass: Understands[ClassContext, AddParent[Type]] =
            new Understands[ClassContext, AddParent[Type]] {
              def perform(
                context: ClassContext,
                command: AddParent[Type]
              ): (ClassContext, Unit) = {
                val resultCls = context.cls.clone()
                resultCls.addExtendedType(command.parentClass.asClassOrInterfaceType())
                (context.copy(cls = resultCls), ())
              }
            }
          implicit val canAddImplementedInClass: Understands[ClassContext, AddImplemented[Type]] =
            new Understands[ClassContext, AddImplemented[Type]] {
              def perform(
                context: ClassContext,
                command: AddImplemented[Type]
              ): (ClassContext, Unit) = {
                val resultCls = context.cls.clone()
                resultCls.addImplementedType(command.interface.asClassOrInterfaceType())
                (context.copy(cls = resultCls), ())
              }
            }
          implicit val canAddFieldInClass: Understands[ClassContext, AddField[Name, Type]] =
            new Understands[ClassContext, AddField[Name, Type]] {
              def perform(
                context: ClassContext,
                command: AddField[Name, Type]
              ): (ClassContext, Unit) = {
                val resultCls = context.cls.clone()
                resultCls.addField(command.tpe, command.name.toAST.toString, Modifier.privateModifier().getKeyword)
                (context.copy(cls = resultCls), ())
              }
            }
          implicit val canAddMethodInClass: Understands[ClassContext, AddMethod[base.MethodBodyContext, Name, Option[Expression]]] =
            new Understands[ClassContext, AddMethod[base.MethodBodyContext, Name, Option[Expression]]] {
              def perform(
                context: ClassContext,
                command: AddMethod[base.MethodBodyContext, Name, Option[Expression]]
              ): (ClassContext, Unit) = {
                val methodToAdd = new MethodDeclaration()
                methodToAdd.setName(command.name.toAST)
                methodToAdd.setPublic(command.isPublic)
                val (methodCtxt, returnExp) =
                  Command.runGenerator(
                    command.spec,
                    MethodBodyCtxt(context.resolver, context.extraImports, methodToAdd)
                  )
                val resultingMethod = methodCtxt.method.clone()
                if (returnExp.isDefined) {
                  val body = resultingMethod.getBody.orElseGet(() => new BlockStmt())
                  body.addStatement(new ReturnStmt(returnExp.get))
                  resultingMethod.setBody(body)
                }
                val resultCls = context.cls.clone()
                resultCls.addMember(resultingMethod)
                val newImports = (context.extraImports ++ methodCtxt.extraImports).distinct.map(_.clone())
                (context.copy(resolver = methodCtxt.resolver, extraImports = newImports, cls = resultCls), ())
              }
            }

          implicit val canAddConstructorInClass: Understands[ClassContext, AddConstructor[ConstructorContext]] =
            new Understands[ClassContext, AddConstructor[ConstructorContext]] {
              def perform(
                context: ClassContext,
                command: AddConstructor[ConstructorContext]
              ): (ClassContext, Unit) = {
                val ctorToAdd = new ConstructorDeclaration()
                ctorToAdd.setPublic(true)
                ctorToAdd.setName(context.cls.getName.clone)
                val (ctorCtxt, _) =
                  Command.runGenerator(
                    command.ctor,
                    CtorCtxt(context.resolver, context.extraImports, ctorToAdd)
                  )
                val resultCls = context.cls.clone()
                resultCls.addMember(ctorCtxt.ctor.clone())
                val newImports = (context.extraImports ++ ctorCtxt.extraImports).distinct.map(_.clone())
                (context.copy(resolver = ctorCtxt.resolver, extraImports = newImports, cls = resultCls), ())
              }
            }

          implicit val canAddImportInClass: Understands[ClassContext, AddImport[Import]] =
            new Understands[ClassContext, AddImport[Import]] {
              def perform(
                context: ClassContext,
                command: AddImport[Import]
              ): (ClassContext, Unit) = {
                val newImports =
                  if (context.extraImports.contains(command.imp)) {
                    context.extraImports
                  } else context.extraImports :+ command.imp.clone()
                (context.copy(extraImports = newImports), ())
              }
            }

          implicit val canResolveImportInClass: Understands[ClassContext, ResolveImport[Import, Type]] =
            new Understands[ClassContext, ResolveImport[Import, Type]] {
              def perform(
                context: ClassContext,
                command: ResolveImport[Import, Type]
              ): (ClassContext, Option[Import]) = {
                Try { (context, context.resolver.importResolution(command.forElem)) } getOrElse {
                  val newImport =
                    new ImportDeclaration(
                      new com.github.javaparser.ast.expr.Name(config.targetPackage.getName.clone(), command.forElem.toString()),
                      false,
                      false)
                  if (context.extraImports.contains(newImport)) {
                    (context, None)
                  } else {
                    (context, Some(newImport))
                  }
                }
              }
            }
          implicit val canSetAbstractInClass: Understands[ClassContext, SetAbstract] =
            new Understands[ClassContext, SetAbstract] {
              def perform(
                context: ClassContext,
                command: SetAbstract
              ): (ClassContext, Unit) = {
                val newClass = context.cls.clone()
                newClass.setAbstract(true)
                (context.copy(cls = newClass), ())
              }
            }
          implicit val canSetInterfaceInClass: Understands[ClassContext, SetInterface] =
            new Understands[ClassContext, SetInterface] {
              def perform(
                context: ClassContext,
                command: SetInterface
              ): (ClassContext, Unit) = {
                val newClass = context.cls.clone()
                newClass.setInterface(true)
                newClass.getMethods.forEach { method =>
                  if (method.getBody.isPresent) {
                    method.setDefault(true)
                  }
                }
                (context.copy(cls = newClass), ())
              }
            }

          implicit val canTranslateTypeInClass: Understands[ClassContext, ToTargetLanguageType[Type]] =
            new Understands[ClassContext, ToTargetLanguageType[Type]] {
              def perform(
                context: ClassContext,
                command: ToTargetLanguageType[Type]
              ): (ClassContext, Type) = {
                Command.runGenerator(context.resolver.classTypeResolution(command.tpe), context)
              }
            }
          implicit val canSelfReferenceInClass: Understands[ClassContext, SelfReference[Expression]] =
            new Understands[ClassContext, SelfReference[Expression]] {
              def perform(
                context: ClassContext,
                command: SelfReference[Expression]
              ): (ClassContext, Expression) = {
                (context, new com.github.javaparser.ast.expr.ThisExpr())
              }
            }
          implicit val canFindClassInClass: Understands[ClassContext, FindClass[Name, Type]] =
            new Understands[ClassContext, FindClass[Name, Type]] {
              def perform(
                context: ClassContext,
                command: FindClass[Name, Type]
              ): (ClassContext, Type) = {
                val result = new ClassOrInterfaceType()
                result.setName(command.name.toAST)
                (context, result)
              }
            }
          implicit val canGetFreshNameInClass: Understands[ClassContext, FreshName[Name]] =
            new Understands[ClassContext, FreshName[Name]] {
              def perform(context: ClassContext, command: FreshName[Name]): (ClassContext, Name) = {
                val freshName = JavaNameProvider.mangle(s"$$$$generatedName_${UUID.randomUUID().toString.replace("-", "_")}$$$$")
                val updatedResolver = context.resolver.copy(
                  generatedVariables = context.resolver.generatedVariables + (freshName.toAST.getIdentifier -> command.basedOn)
                )
                (context.copy(resolver = updatedResolver), freshName)
              }
            }
        }

      val constructorCapabilities: ConstructorCapabilities =
        new ConstructorCapabilities {
          implicit val canInitializeParentInConstructor: Understands[ConstructorContext, InitializeParent[Expression]] =
            new Understands[ConstructorContext, InitializeParent[Expression]] {
              def perform(
                context: ConstructorContext,
                command: InitializeParent[Expression]
              ): (ConstructorContext, Unit) = {
                val newCtor = context.ctor.clone()
                val superCall =
                  newCtor.getBody
                    .findFirst(classOf[ExplicitConstructorInvocationStmt])
                    .orElseGet(() => new ExplicitConstructorInvocationStmt())
                superCall.setThis(false)
                superCall.removeExpression()
                superCall.getArguments.clear()
                command.arguments.foreach { arg =>
                  superCall.addArgument(arg.clone())
                }
                (context.copy(ctor = newCtor), ())
              }
            }
          implicit val canInitializeFieldInConstructor: Understands[ConstructorContext, InitializeField[Name, Expression]] =
            new Understands[ConstructorContext, InitializeField[Name, Expression]] {
              def perform(
                context: ConstructorContext,
                command: InitializeField[Name, Expression]
              ): (ConstructorContext, Unit) = {
                Command.runGenerator(
                  addBlockDefinitions(Java(s"this.${command.name} = ${command.value};").statements()),
                  context)
              }
            }
          implicit val canAddBlockDefinitionsInConstructor: Understands[ConstructorContext, AddBlockDefinitions[Statement]] =
            new Understands[ConstructorContext, AddBlockDefinitions[Statement]] {
              def perform(
                context: CtorCtxt,
                command: AddBlockDefinitions[Statement]
              ): (CtorCtxt, Unit) = {
                val newCtor = context.ctor.clone()
                val body = newCtor.getBody
                command.definitions.foreach(stmt => body.addStatement(stmt))
                (context.copy(ctor = newCtor), ())
              }
            }
          implicit val canAddImportInConstructor: Understands[ConstructorContext, AddImport[Import]] =
            new Understands[ConstructorContext, AddImport[Import]] {
              def perform(
                context: ConstructorContext,
                command: AddImport[Import]
              ): (ConstructorContext, Unit) =
                (context.copy(extraImports = (context.extraImports :+ command.imp).distinct.map(_.clone())), ())
            }
          implicit val canResolveImportInConstructor: Understands[ConstructorContext, ResolveImport[Import, Type]] =
            new Understands[ConstructorContext, ResolveImport[ImportDeclaration, Type]] {
              def perform(
                context: ConstructorContext,
                command: ResolveImport[ImportDeclaration, Type]
              ): (ConstructorContext, Option[ImportDeclaration]) = {
                Try { (context, context.resolver.importResolution(command.forElem)) } getOrElse {
                  val newImport =
                    new ImportDeclaration(
                      new com.github.javaparser.ast.expr.Name(config.targetPackage.getName.clone(), command.forElem.toString()),
                      false,
                      false)
                  if (context.extraImports.contains(newImport)) {
                    (context, None)
                  } else {
                    (context, Some(newImport))
                  }
                }
              }
            }
          implicit val canInstantiateObjectInConstructor: Understands[ConstructorContext, InstantiateObject[Type, Expression]] =
            new Understands[ConstructorContext, InstantiateObject[Type, Expression]] {
              def perform(
                context: ConstructorContext,
                command: InstantiateObject[Type, Expression]
              ): (ConstructorContext, Expression) = {
                val (tpe, args) = context.resolver.instantiationOverride(command.tpe, command.constructorArguments)
                (context, Java(s"""new ${tpe}(${args.mkString(", ")})""").expression())
              }
            }
          implicit val canApplyInConstructor: Understands[ConstructorContext, Apply[Expression, Expression, Expression]] =
            new Understands[ConstructorContext, Apply[Expression, Expression, Expression]] {
              def perform(
                context: ConstructorContext,
                command: Apply[Expression, Expression, Expression]
              ): (ConstructorContext, Expression) = {
                (context, Java(s"${command.functional}(${command.arguments.mkString(", ")})").expression())
              }
            }

          implicit val canGetMemberInConstructor: Understands[ConstructorContext, GetMember[Expression, Name]] =
            new Understands[ConstructorContext, GetMember[Expression, Name]] {
              def perform(
                context: ConstructorContext,
                command: GetMember[Expression, Name]
              ): (ConstructorContext, Expression) = {
                (context, Java(s"""${command.instance}.${command.member}""").expression())
              }
            }
          implicit val canSelfReferenceInConstructor: Understands[ConstructorContext, SelfReference[Expression]] =
            new Understands[ConstructorContext, SelfReference[Expression]] {
              def perform(
                context: ConstructorContext,
                command: SelfReference[Expression]
              ): (ConstructorContext, Expression) = {
                (context, new com.github.javaparser.ast.expr.ThisExpr())
              }
            }

          implicit val canGetArgumentsInConstructor: Understands[ConstructorContext, GetArguments[Type, Name, Expression]] =
            new Understands[ConstructorContext, GetArguments[Type, Name, Expression]] {
              def perform(
                context: ConstructorContext,
                command: GetArguments[Type, Name, Expression]
              ): (ConstructorContext, Seq[(Name, Type, Expression)]) = {
                val args = context.ctor.getParameters.asScala.map { param =>
                  (MangledName.fromAST(param.getName), param.getType.clone(), new NameExpr(param.getName.clone()))
                }
                (context, args)
              }
            }
          implicit val canTranslateTypeInConstructor: Understands[ConstructorContext, ToTargetLanguageType[Type]] =
            new Understands[ConstructorContext, ToTargetLanguageType[Type]] {
              def perform(
                context: ConstructorContext,
                command: ToTargetLanguageType[Type]
              ): (ConstructorContext, Type) = {
                Command.runGenerator(context.resolver.constructorTypeResolution(command.tpe), context)
              }
            }
          implicit def canReifyInConstructor[T]: Understands[ConstructorContext, Reify[T, Expression]] =
            new Understands[ConstructorContext, Reify[T, Expression]] {
              def perform(
                context: ConstructorContext,
                command: Reify[T, Expression]
              ): (ConstructorContext, Expression) = {
                Command.runGenerator(context.resolver.reificationInConstructor(InstanceRep(command.tpe)(command.value)), context)
              }
            }
          implicit val canSetParametersInConstructor: Understands[ConstructorContext, SetParameters[Name, Type]] =
            new Understands[ConstructorContext, SetParameters[Name, Type]] {
              def perform(
                context: ConstructorContext,
                command: SetParameters[Name, Type]
              ): (ConstructorContext, Unit) = {
                val newCtor = context.ctor.clone()
                newCtor.getParameters.clear()
                command.params.foreach { case (name, tpe) =>
                  newCtor.addParameter(tpe, name.toAST.toString)
                }
                (context.copy(ctor = newCtor), ()) // second thing to be returned isn't optional, so make it () is like Unit
              }
            }

          implicit val canGetConstructorInConstructor: Understands[ConstructorContext, GetConstructor[Type, Expression]] =
            new Understands[ConstructorContext, GetConstructor[Type, Expression]] {
              def perform(
                context: ConstructorContext,
                command: GetConstructor[Type, Expression]
              ): (ConstructorContext, Expression) = {
                (context, Java(command.tpe).expression())
              }
            }
          implicit val canFindClassInConstructor: Understands[ConstructorContext, FindClass[Name, Type]] =
            new Understands[ConstructorContext, FindClass[Name, Type]] {
              def perform(
                context: ConstructorContext,
                command: FindClass[Name, Type]
              ): (ConstructorContext, Type) = {
                val result = new ClassOrInterfaceType()
                result.setName(command.name.toAST)
                (context, result)
              }
            }
          implicit val canGetFreshNameInConstructor: Understands[ConstructorContext, FreshName[Name]] =
            new Understands[ConstructorContext, FreshName[Name]] {
              def perform(context: ConstructorContext, command: FreshName[MangledName]): (ConstructorContext, MangledName) = {
                val freshName = JavaNameProvider.mangle(s"$$$$generatedName_${UUID.randomUUID().toString.replace("-", "_")}$$$$")
                val updatedResolver = context.resolver.copy(
                  generatedVariables = context.resolver.generatedVariables + (freshName.toAST.getIdentifier -> command.basedOn)
                )
                (context.copy(resolver = updatedResolver), freshName)
              }
            }
        }
      val methodBodyCapabilities: MethodBodyCapabilities =
        new MethodBodyCapabilities {
          import base._
          val canInstantiateObjectInMethod: Understands[MethodBodyContext, InstantiateObject[Type, Expression]] =
            new Understands[MethodBodyContext, InstantiateObject[Type, Expression]] {
              def perform(
                context: MethodBodyContext,
                command: InstantiateObject[Type, Expression]
              ): (MethodBodyContext, Expression) = {
                val (tpe, args) = context.resolver.instantiationOverride(command.tpe, command.constructorArguments)
                (context, Java(s"""new ${tpe}(${args.mkString(", ")})""").expression())
              }
            }
          val canGetMemberInMethod: Understands[MethodBodyContext, GetMember[Expression, Name]] =
            new Understands[MethodBodyContext, GetMember[Expression, Name]] {
              def perform(
                context: MethodBodyContext,
                command: GetMember[Expression, Name]
              ): (MethodBodyContext, Expression) = {
                (context, Java(s"""${command.instance}.${command.member}""").expression())
              }
            }
          val canSetAbstractInMethod: Understands[MethodBodyContext, SetAbstract] =
            new Understands[MethodBodyContext, SetAbstract] {
              def perform(
                context: MethodBodyContext,
                command: SetAbstract
              ): (MethodBodyContext, Unit) = {
                val newMethod = context.method.clone()
                newMethod.removeBody()
                newMethod.setAbstract(true)
                (context.copy(method = newMethod), ())
              }
            }
          val canSelfReferenceInMethod: Understands[MethodBodyContext, SelfReference[Expression]] =
            new Understands[MethodBodyContext, SelfReference[Expression]] {
              def perform(
                context: MethodBodyContext,
                command: SelfReference[Expression]
              ): (MethodBodyContext, Expression) = {
                (context, new com.github.javaparser.ast.expr.ThisExpr())
              }
            }
          val canGetConstructorInMethod: Understands[MethodBodyContext, GetConstructor[Type, Expression]] =
            new Understands[MethodBodyContext, GetConstructor[Type, Expression]] {
              def perform(
                context: MethodBodyContext,
                command: GetConstructor[Type, Expression]
              ): (MethodBodyContext, Expression) = {
                (context, Java(command.tpe).expression())
              }
            }
          val canFindClassInMethod: Understands[MethodBodyContext, FindClass[Name, Type]] =
            new Understands[MethodBodyContext, FindClass[Name, Type]] {
              def perform(
                context: MethodBodyContext,
                command: FindClass[Name, Type]
              ): (MethodBodyContext, Type) = {
                val result = new ClassOrInterfaceType()
                result.setName(command.name.toAST)
                (context, result)
              }
            }
        }
      val projectCapabilities: ProjectCapabilities =
        new ProjectCapabilities {
          import base._
          implicit val canAddTypeLookupForClassesInProject: Understands[ProjectContext, AddTypeLookup[ClassContext, Type]] =
            new Understands[ProjectContext, AddTypeLookup[ClassContext, Type]] {
              def perform(
                context: ProjectContext,
                command: AddTypeLookup[ClassContext, Type]
              ): (ProjectContext, Unit) = {
                def newLookup(tpe: TypeRep): Generator[ClassContext, Type] =
                  if (tpe == command.tpe) {
                    command.lookup
                  } else {
                    context.resolver.classTypeResolution(tpe)
                  }
                (context.copy(resolver = context.resolver.copy(classTypeResolution = newLookup)), ())
              }
            }
          implicit val canAddTypeLookupForConstructorsInProject: Understands[ProjectContext, AddTypeLookup[ConstructorContext, Type]] =
            new Understands[ProjectContext, AddTypeLookup[ConstructorContext, Type]] {
              def perform(
                context: ProjectContext,
                command: AddTypeLookup[ConstructorContext, Type]
              ): (ProjectContext, Unit) = {
                def newLookup(tpe: TypeRep): Generator[ConstructorContext, Type] =
                  if (tpe == command.tpe) {
                    command.lookup
                  } else {
                    context.resolver.constructorTypeResolution(tpe)
                  }
                (context.copy(resolver = context.resolver.copy(constructorTypeResolution = newLookup)), ())
              }
            }
        }
    }

  trait BlockContextManipulator[Ctxt] {
    def getBlock(ctxt: Ctxt): BlockStmt
    def copyWithBlock(ctxt: Ctxt, blockStmt: BlockStmt): Ctxt
    def nextBlockContext(ctxt: Ctxt): Ctxt = copyWithBlock(ctxt, new BlockStmt())
  }

  protected class ContextIndependentImperativeCapabilities[Ctxt](manip: BlockContextManipulator[Ctxt]) extends Imperative[Ctxt] {
    val base: paradigm.type = paradigm
    object imperativeCapabilities extends ImperativeCapabilities {
      implicit val canDeclareVariable: Understands[Ctxt, DeclareVariable[Name, Type, Option[Expression], Expression]] =
        new Understands[Ctxt, DeclareVariable[Name, Type, Option[Expression], Expression]] {
          def perform(context: Ctxt, command: DeclareVariable[Name, Type, Option[Expression], Expression]): (Ctxt, Expression) = {
            val decl = new VariableDeclarationExpr(command.tpe, command.name.toAST.toString)
            val withAssignment =
              if (command.initialization.isDefined) {
                new AssignExpr(decl, command.initialization.get.clone(), AssignExpr.Operator.ASSIGN)
              } else {
                decl
              }
            val nextBlock = manip.getBlock(context).clone()
            nextBlock.addStatement(withAssignment)
            (manip.copyWithBlock(context, nextBlock), new NameExpr(command.name.toAST))
          }
        }

      implicit val canAssignVariable: Understands[Ctxt, AssignVariable[Expression, Statement]] =
        new Understands[Ctxt, AssignVariable[Expression, Statement]] {
          def perform(context: Ctxt, command: AssignVariable[Expression, Statement]): (Ctxt, Statement) = {
            (context, new ExpressionStmt(new AssignExpr(command.variable.clone(), command.value.clone(), AssignExpr.Operator.ASSIGN)))
          }
        }

      implicit val canReturn: Understands[Ctxt, Return[Expression, Statement]] =
        new Understands[Ctxt, Return[Expression, Statement]] {
          def perform(context: Ctxt, command: Return[Expression, Statement]): (Ctxt, Statement) = {
            (context, new ReturnStmt(command.exp.clone()))
          }
        }

      implicit val canIfThenElse: Understands[Ctxt, IfThenElse[Expression, Generator[Ctxt, Unit], Option[Generator[Ctxt, Unit]], Statement]] =
        new Understands[Ctxt, IfThenElse[Expression, Generator[Ctxt, Unit], Option[Generator[Ctxt, Unit]], Statement]] {
          def perform(
            context: Ctxt,
            command: IfThenElse[Expression, Generator[Ctxt, Unit], Option[Generator[Ctxt, Unit]], Statement]
          ): (Ctxt, Statement) = {
            val (startCtxt, _) = Command.runGenerator(command.ifBranch, manip.nextBlockContext(context))
            val iteStmt: IfStmt = new IfStmt()
            iteStmt.setCondition(command.condition)
            iteStmt.setThenStmt(manip.getBlock(startCtxt))
            val (beforeElseCtxt, beforeElseIteStmt) =
              command.elseIfBranches.foldLeft((manip.nextBlockContext(startCtxt), iteStmt)) {
                case ((ctxt, iteStmt), (cond, gen)) =>
                  val (nextCtxt, _) = Command.runGenerator(gen, manip.nextBlockContext(ctxt))
                  val nextIte = new IfStmt()
                  nextIte.setCondition(cond)
                  nextIte.setThenStmt(manip.getBlock(nextCtxt))
                  iteStmt.setElseStmt(nextIte)
                  (nextCtxt, nextIte)
              }
            val afterElseCtxt =
              command.elseBranch
                .map { elseGen =>
                  val (resCtxt, _) = Command.runGenerator(elseGen, manip.nextBlockContext(beforeElseCtxt))
                  beforeElseIteStmt.setElseStmt(manip.getBlock(resCtxt))
                  resCtxt
                }.getOrElse(beforeElseCtxt)
            (manip.copyWithBlock(afterElseCtxt, manip.getBlock(context)), iteStmt.clone())
          }
        }
      implicit val canWhile: Understands[Ctxt, While[Ctxt, Expression, Statement]] =
        new Understands[Ctxt, While[Ctxt, Expression, Statement]] {
          def perform(context: Ctxt, command: While[Ctxt, Expression, Statement]): (Ctxt, Statement) = {
            val (whileCtxt, _) = Command.runGenerator(command.block, manip.nextBlockContext(context))
            val whileStmt = new WhileStmt()
            whileStmt.setCondition(command.condition)
            whileStmt.setBody(manip.getBlock(whileCtxt))
            (manip.copyWithBlock(whileCtxt, manip.getBlock(context)), whileStmt)
          }
        }
    }
  }

  val imperativeInMethod = new ContextIndependentImperativeCapabilities[MethodBodyCtxt](
    new BlockContextManipulator[MethodBodyCtxt] {
      def getBlock(ctxt: MethodBodyCtxt): BlockStmt = ctxt.method.getBody.get()
      def copyWithBlock(ctxt: MethodBodyCtxt, blockStmt: BlockStmt): MethodBodyCtxt = {
        val newMethod = ctxt.method.clone()
        newMethod.setBody(blockStmt)
        ctxt.copy(method = newMethod)
      }
    }
  )

  val imperativeInConstructor = new ContextIndependentImperativeCapabilities[CtorCtxt](
    new BlockContextManipulator[CtorCtxt] {
      def getBlock(ctxt: CtorCtxt): BlockStmt = ctxt.ctor.getBody
      def copyWithBlock(ctxt: CtorCtxt, blockStmt: BlockStmt): CtorCtxt = {
        val newCtor = ctxt.ctor.clone()
        newCtor.setBody(blockStmt.clone())
        ctxt.copy(ctor = newCtor)
      }
    }
  )

  object parametricPolymorphism extends ParametricPolymorphism {
    val base: paradigm.type = paradigm
    type TypeParameterContext = TypeParamCtxt
    import base._
    val methodBodyCapabilities: MethodBodyCapabilities =
      new MethodBodyCapabilities {
        implicit val canAddTypeParameterInMethod: Understands[MethodBodyContext, AddTypeParameter[Name, TypeParameterContext]] =
          new Understands[MethodBodyContext, AddTypeParameter[Name, TypeParameterContext]] {
            def perform(context: MethodBodyContext, command: AddTypeParameter[MangledName, TypeParamCtxt]): (MethodBodyContext, Unit) = {
              val (resultCtxt, _) = Command.runGenerator(command.spec, TypeParamCtxt(new TypeParameter()))
              val tpeParam = resultCtxt.param.clone()
              tpeParam.setName(command.name.toAST)
              val newMethod = context.method.clone()
              newMethod.addTypeParameter(tpeParam)
              (context.copy(method = newMethod), ())
            }
          }
        implicit val canGetTypeArgumentsInMethod: Understands[MethodBodyContext, GetTypeArguments[Type]] =
          new Understands[MethodBodyContext, GetTypeArguments[Type]] {
            def perform(context: MethodBodyContext, command: GetTypeArguments[Type]): (MethodBodyContext, Seq[Type]) = {
              (context, context.method.getTypeParameters.asScala)
            }
          }
        implicit val canApplyTypeInMethod: Understands[MethodBodyContext, Apply[Type, Type, Type]] =
          new Understands[MethodBodyContext, Apply[Type, Type, Type]] {
            def perform(context: MethodBodyContext, command: Apply[Type, Type, Type]): (MethodBodyContext, Type) = {
              val resultTpe = command.functional.clone().asClassOrInterfaceType()
              val boxedArguments = command.arguments.map { arg =>
                if (arg.isPrimitiveType) arg.asPrimitiveType().toBoxedType
                else arg.clone()
              }
              resultTpe.setTypeArguments(boxedArguments: _*)
              (context, resultTpe)
            }
          }
        implicit val canApplyMethodToTypeInMethod: Understands[MethodBodyContext, Apply[Expression, Type, Expression]] =
          new Understands[MethodBodyContext, Apply[Expression, Type, Expression]] {
            def perform(context: MethodBodyContext, command: Apply[Expression, Type, Expression]): (MethodBodyContext, Expression) = {
              val resultExp =
                if (command.functional.isMethodCallExpr) {
                  command.functional.clone().asMethodCallExpr()
                } else {
                  Java(s"${command.functional}()").expression[MethodCallExpr]()
                }
              val boxedArguments = command.arguments.map { arg =>
                if (arg.isPrimitiveType) arg.asPrimitiveType().toBoxedType
                else arg.clone()
              }
              resultExp.setTypeArguments(boxedArguments: _*)
              (context, resultExp)
            }
          }
      }
  }

  object generics extends Generics {
    val base: paradigm.type = paradigm 
    val ooParadigm: cc.ooParadigm.type  = cc.ooParadigm
    val ppolyParadigm: parametricPolymorphism.type  = parametricPolymorphism
    import ooParadigm._
    import ppolyParadigm._    
    val classCapabilities: ClassCapabilities =
      new ClassCapabilities {
        implicit val canAddTypeParameterInClass: Understands[ClassContext, AddTypeParameter[Name, TypeParameterContext]] =
          new Understands[ClassContext, AddTypeParameter[Name, TypeParameterContext]] {
            def perform(context: ClassContext, command: AddTypeParameter[Name, TypeParameterContext]): (ClassContext, Unit) = {
              val (resultCtxt, _) = Command.runGenerator(command.spec, TypeParamCtxt(new TypeParameter()))
              val tpeParam = resultCtxt.param.clone()
              tpeParam.setName(command.name.toAST)
              val newCls = context.cls.clone()
              newCls.addTypeParameter(tpeParam)
              (context.copy(cls = newCls), ())
            }
          }
        implicit val canGetTypeArgumentsInClass: Understands[ClassContext, GetTypeArguments[Type]] =
          new Understands[ClassContext, GetTypeArguments[Type]] {
            def perform(context: ClassContext, command: GetTypeArguments[Type]): (ClassContext, Seq[Type]) = {
              (context, context.cls.getTypeParameters.asScala)
            }
          }
        implicit val canApplyTypeInClass: Understands[ClassContext, Apply[Type, Type, Type]] =
          new Understands[ClassContext, Apply[Type, Type, Type]] {
            def perform(context: ClassContext, command: Apply[Type, Type, Type]): (ClassContext, Type) = {
              val resultTpe = command.functional.clone().asClassOrInterfaceType()
              val boxedArguments = command.arguments.map { arg =>
                if (arg.isPrimitiveType) arg.asPrimitiveType().toBoxedType
                else arg.clone()
              }
              resultTpe.setTypeArguments(boxedArguments: _*)
              (context, resultTpe)
            }
          }
      }
    val typeParameterCapabilities: TypeParameterCapabilities =
      new TypeParameterCapabilities {
        implicit val canAddUpperBoundInTypeParameter: Understands[TypeParameterContext, AddUpperBound[Type]] =
          new Understands[TypeParameterContext, AddUpperBound[Type]] {
            def perform(context: TypeParameterContext, command: AddUpperBound[Type]): (TypeParameterContext, Unit) = {
              throw new UnsupportedOperationException("Sorry, Java does not support upper bounds on type parameters.")
            }
          }
        implicit val canAddLowerBoundInTypeParameter: Understands[TypeParameterContext, AddLowerBound[Type]] =
          new Understands[TypeParameterContext, AddLowerBound[Type]] {
            def perform(context: TypeParameterContext, command: AddLowerBound[Type]): (TypeParameterContext, Unit) = {
              val newParam = context.param.clone()
              newParam.getTypeBound.add(command.bound.toClassOrInterfaceType().get().clone())
              (context.copy(param = newParam), ())
            }
          }
        implicit val canApplyTypeTypeParameter: Understands[TypeParameterContext, Apply[Type, Type, Type]] =
          new Understands[TypeParameterContext, Apply[Type, Type, Type]] {
            def perform(context: TypeParameterContext, command: Apply[Type, Type, Type]): (TypeParameterContext, Type) = {
              val resultTpe = command.functional.clone().asClassOrInterfaceType()
              val boxedArguments = command.arguments.map { arg =>
                if (arg.isPrimitiveType) arg.asPrimitiveType().toBoxedType
                else arg.clone()
              }
              resultTpe.setTypeArguments(boxedArguments: _*)
              (context, resultTpe)
            }
          }
      }
    val constructorCapabilities: ConstructorCapabilities =
      new ConstructorCapabilities {
        implicit val canApplyTypeInConstructor: Understands[ConstructorContext, Apply[Type, Type, Type]] =
          new Understands[ConstructorContext, Apply[Type, Type, Type]] {
            def perform(context: ConstructorContext, command: Apply[Type, Type, Type]): (ConstructorContext, Type) = {
              val resultTpe = command.functional.clone().asClassOrInterfaceType()
              val boxedArguments = command.arguments.map { arg =>
                if (arg.isPrimitiveType) arg.asPrimitiveType().toBoxedType
                else arg.clone()
              }
              resultTpe.setTypeArguments(boxedArguments: _*)
              (context, resultTpe)
            }
          }
        implicit val canApplyMethodToTypeInConstructor: Understands[ConstructorContext, Apply[Expression, Type, Expression]] =
          new Understands[ConstructorContext, Apply[Expression, Type, Expression]] {
            def perform(context: ConstructorContext, command: Apply[Expression, Type, Expression]): (ConstructorContext, Expression) = {
              val resultExp = command.functional.clone().asMethodReferenceExpr()
              val boxedArguments = command.arguments.map { arg =>
                if (arg.isPrimitiveType) arg.asPrimitiveType().toBoxedType
                else arg.clone()
              }
              resultExp.setTypeArguments(boxedArguments: _*)
              (context, resultExp)
            }
          }
      }
  }

  def infixExprOp[Ctxt, Op](infixOp: BinaryExpr.Operator): Understands[Ctxt, Apply[Op, Expression, Expression]] =
    new Understands[Ctxt, Apply[Op, Expression, Expression]] {
      def perform(
        context: Ctxt,
        command: Apply[Op, Expression, Expression]
      ): (Ctxt, Expression) = {
        (context, new BinaryExpr(command.arguments(0), command.arguments(1), infixOp))
      }
    }

  def shortCutInfixExprOp[Ctxt, Op <: { val shortcut: Boolean }](
    shortCutOp: BinaryExpr.Operator,
    normalOp: BinaryExpr.Operator
  ): Understands[Ctxt, Apply[Op, Expression, Expression]] =
    new Understands[Ctxt, Apply[Op, Expression, Expression]] {
      def perform(
        context: Ctxt,
        command: Apply[Op, Expression, Expression]
      ): (Ctxt, Expression) = {
        import scala.language.reflectiveCalls
        if (command.functional.shortcut) {
          (context, new BinaryExpr(command.arguments(0), command.arguments(1), shortCutOp))
        } else {
          (context, new BinaryExpr(command.arguments(0), command.arguments(1), normalOp))
        }
      }
    }

  def prefixExprOp[Ctxt, Op](infixOp: UnaryExpr.Operator): Understands[Ctxt, Apply[Op, Expression, Expression]] =
    new Understands[Ctxt, Apply[Op, Expression, Expression]] {
      def perform(
        context: Ctxt,
        command: Apply[Op, Expression, Expression]
      ): (Ctxt, Expression) = {
        (context, new UnaryExpr(command.arguments(0), infixOp))
      }
    }

  private case object Enable extends Command {
    type Result = Unit
  }

  private def updateResolver
    (rep: TypeRep, translateTo: Type, extraImport: Option[ImportDeclaration] = None)
    (reification: rep.HostType => Expression): ContextSpecificResolver => ContextSpecificResolver =
    resolver => {
    def possiblyBoxedTargetType(needsBox: Boolean): Type = {
      if (needsBox && translateTo.isPrimitiveType) {
        translateTo.asPrimitiveType().toBoxedType
      } else translateTo
    }

    def addResolutionType[Ctxt](
      targetType: Type,
      toResolution: TypeRep => Generator[Ctxt, Type]
    ): TypeRep => Generator[Ctxt, Type] = {
      case r if r == rep => Command.lift(targetType)
      case other => toResolution(other)
    }

    def addReification[Ctxt](
      reify: InstanceRep => Generator[Ctxt, Expression]
    ): InstanceRep => Generator[Ctxt, Expression] = {
      case instRep if instRep.tpe == rep =>
        Command.lift(reification(instRep.inst.asInstanceOf[rep.HostType]))
      case other => reify(other)
    }

    def addExtraImport(
      importResolution: Type => Option[Import]
    ): Type => Option[Import] = {
      case r if r == translateTo || r == possiblyBoxedTargetType(true) => extraImport
      case other => importResolution(other)
    }

    resolver.copy(
      methodTypeResolution =
        addResolutionType(
          possiblyBoxedTargetType(config.boxLevel.inMethods),
          resolver.methodTypeResolution
        ),
      constructorTypeResolution =
        addResolutionType(
          possiblyBoxedTargetType(config.boxLevel.inConstructors),
          resolver.constructorTypeResolution
        ),
      classTypeResolution =
        addResolutionType(
          possiblyBoxedTargetType(config.boxLevel.inClasses),
          resolver.classTypeResolution
        ),
      reificationInConstructor = addReification(resolver.reificationInConstructor),
      reificationInMethod = addReification(resolver.reificationInMethod),
      importResolution = addExtraImport(resolver.importResolution)
    )
  }

  class ContextIndependentBooleans[Ctxt] extends Booleans[Ctxt] {
    val base: paradigm.type = paradigm
    val booleanCapabilities: BooleanCapabilities =
    new BooleanCapabilities {
      implicit val canAnd: Understands[Ctxt, Apply[And, base.syntax.Expression, base.syntax.Expression]] =
        shortCutInfixExprOp[Ctxt, And](BinaryExpr.Operator.AND, BinaryExpr.Operator.BINARY_AND)
      implicit val canOr: Understands[Ctxt, Apply[Or, base.syntax.Expression, base.syntax.Expression]] =
        shortCutInfixExprOp[Ctxt, Or](BinaryExpr.Operator.OR, BinaryExpr.Operator.BINARY_OR)
      implicit val canNot: Understands[Ctxt, Apply[Not, base.syntax.Expression, base.syntax.Expression]] =
        prefixExprOp[Ctxt, Not](UnaryExpr.Operator.LOGICAL_COMPLEMENT)
      implicit val canTrue: Understands[Ctxt, True[base.syntax.Expression]] =
        new Understands[Ctxt, True[base.syntax.Expression]] {
          def perform(
            context: Ctxt,
            command: True[Expression]
          ): (Ctxt, Expression) = {
            (context, new BooleanLiteralExpr(true))
          }
        }
      implicit val canFalse: Understands[Ctxt, False[base.syntax.Expression]] =
        new Understands[Ctxt, False[base.syntax.Expression]] {
          def perform(
            context: Ctxt,
            command: False[Expression]
          ): (Ctxt, Expression) = {
            (context, new BooleanLiteralExpr(false))
          }
        }
    }
    def enable(): Generator[base.ProjectContext, Unit] =
      Enable.interpret(new Understands[base.ProjectContext, Enable.type] {
        def perform(
          context: ProjectCtxt,
          command: Enable.type
        ): (ProjectCtxt, Unit) = {
          val resolverUpdate =
            updateResolver(TypeRep.Boolean, Java("boolean").tpe())(new BooleanLiteralExpr(_))
          (context.copy(resolver = resolverUpdate(context.resolver)), ())
        }
      })
  }

  val booleansInMethod = new ContextIndependentBooleans[MethodBodyCtxt]
  val booleansInConstructor = new ContextIndependentBooleans[MethodBodyCtxt]

  class ContextIndependentArith[Ctxt, T](
    rep: TypeRep.OfHostType[T],
    targetType: Type,
    reification: T => Expression
  ) extends Arithmetic[Ctxt, T] {
    val base: paradigm.type = paradigm
    val arithmeticCapabilities: ArithmeticCapabilities =
      new ArithmeticCapabilities {
        implicit val canAdd: Understands[Ctxt, Apply[Add[T], Expression, Expression]] =
          infixExprOp(BinaryExpr.Operator.PLUS)
        implicit val canSub: Understands[Ctxt, Apply[Sub[T], Expression, Expression]] =
          infixExprOp(BinaryExpr.Operator.MINUS)
        implicit val canMult: Understands[Ctxt, Apply[Mult[T], Expression, Expression]] =
          infixExprOp(BinaryExpr.Operator.MULTIPLY)
        implicit val canDiv: Understands[Ctxt, Apply[Div[T], Expression, Expression]] =
          infixExprOp(BinaryExpr.Operator.DIVIDE)
        implicit val canMod: Understands[Ctxt, Apply[Mod[T], Expression, Expression]] =
          infixExprOp(BinaryExpr.Operator.REMAINDER)
      }
    def enable(): Generator[base.ProjectContext, Unit] =
      Enable.interpret(new Understands[base.ProjectContext, Enable.type] {
        def perform(
          context: ProjectCtxt,
          command: Enable.type
        ): (ProjectCtxt, Unit) = {
          val resolverUpdate =
            updateResolver(rep, targetType)(reification)(_)
          (context.copy(resolver = resolverUpdate(context.resolver)), ())
        }
      })
  }

  val doublesInMethod =
    new ContextIndependentArith[MethodBodyCtxt, Double](
      TypeRep.Double,
      Java("double").tpe(),
      new DoubleLiteralExpr(_)
    )

  val doublesInConstructor =
    new ContextIndependentArith[CtorCtxt, Double](
      TypeRep.Double,
      Java("double").tpe(),
      new DoubleLiteralExpr(_)
    )

  class ContextIndependentStrings[Ctxt](
    getMember: Understands[Ctxt, GetMember[Expression, Name]],
    applyMethod: Understands[Ctxt, Apply[Expression, Expression, Expression]]
  ) extends Strings[Ctxt] {
    val base: paradigm.type = paradigm
    val stringCapabilities: StringCapabilities =
      new StringCapabilities {
        implicit val canGetStringLength: Understands[Ctxt, Apply[GetStringLength, Expression, Expression]] =
          new Understands[Ctxt, Apply[GetStringLength, Expression, Expression]] {
            def perform(
              context: Ctxt,
              command: Apply[GetStringLength, Expression, Expression]
            ): (Ctxt, Expression) = {
              implicit val _getMember = getMember
              implicit val _applyMethod = applyMethod
              val gen = for {
                lengthMethod <- GetMember[Expression, Name](command.arguments(0), JavaNameProvider.mangle("length")).interpret
                res <- Apply[Expression, Expression, Expression](lengthMethod, Seq.empty).interpret
              } yield res
              Command.runGenerator(gen, context)
            }
          }
        implicit val canAppend: Understands[Ctxt, Apply[StringAppend, Expression, Expression]] =
          new Understands[Ctxt, Apply[StringAppend, Expression, Expression]] {
            def perform(
              context: Ctxt,
              command: Apply[StringAppend, Expression, Expression]
            ): (Ctxt, Expression) = {
              (context, command.arguments.tail.foldLeft(command.arguments.head){ case (str, next) =>
                new BinaryExpr(str, next, BinaryExpr.Operator.PLUS)
              })
            }
          }
        implicit val canToStringInCtxt: Understands[Ctxt, Apply[ToString[Type], Expression, Expression]] =
          new Understands[Ctxt, Apply[ToString[Type], Expression, Expression]] {
            def perform(
              context: Ctxt,
              command: Apply[ToString[Type], Expression, Expression]
            ): (Ctxt, Expression) = {
              implicit val _getMember = getMember
              implicit val _applyMethod = applyMethod
              val gen = Command.lift[Ctxt, Expression](Java(s"String.valueOf(${command.arguments.head})").expression())
              Command.runGenerator(gen, context)
            }
          }
      }

    def enable(): Generator[base.ProjectContext, Unit] =
      Enable.interpret(new Understands[base.ProjectContext, Enable.type] {
        def perform(
          context: ProjectCtxt,
          command: Enable.type
        ): (ProjectCtxt, Unit) = {
          val resolverUpdate =
            updateResolver(TypeRep.String, Java("String").tpe())(new StringLiteralExpr(_))
          (context.copy(resolver = resolverUpdate(context.resolver)), ())
        }
      })
  }

  val stringsInMethod =
    new ContextIndependentStrings[MethodBodyCtxt](
      ooParadigm.methodBodyCapabilities.canGetMemberInMethod,
      paradigm.methodBodyCapabilities.canApplyInMethodBody
    )

  val stringsInConstructor =
    new ContextIndependentStrings[CtorCtxt](
      ooParadigm.constructorCapabilities.canGetMemberInConstructor,
      ooParadigm.constructorCapabilities.canApplyInConstructor
    )

  class ContextIndependentEquality[Ctxt](
    getMember: Understands[Ctxt, GetMember[Expression, Name]],
    applyMethod: Understands[Ctxt, Apply[Expression, Expression, Expression]]
  ) extends Equality[Ctxt] {
    val base: paradigm.type = paradigm
    val equalityCapabilities: EqualityCapabilities =
      new EqualityCapabilities {
        implicit val canEquals: Understands[Ctxt, Apply[ffi.Equals[Type], Expression, Expression]] =
          new Understands[Ctxt, Apply[ffi.Equals[Type], Expression, Expression]] {
            def perform(
              context: Ctxt,
              command: Apply[ffi.Equals[Type], Expression, Expression]
            ): (Ctxt, Expression) = {
              val tpe = command.functional.inType.toClassOrInterfaceType
              if (tpe.isPresent) {
                implicit val _getMember = getMember
                implicit val _applyMethod = applyMethod
                val boxedLhs =
                  if (!config.boxLevel.isConsistent && tpe.get.isBoxedType) {
                    Java(s"${tpe.get}.valueOf(${command.arguments.head})").expression()
                  } else {
                    command.arguments.head
                  }
                val gen = for {
                  equalsMethod <- GetMember[Expression, Name](boxedLhs, JavaNameProvider.mangle("equals")).interpret
                  res <- Apply[Expression, Expression, Expression](equalsMethod, command.arguments.tail).interpret
                } yield res
                Command.runGenerator(gen, context)
              } else {
                (context, new BinaryExpr(command.arguments(0), command.arguments(1), BinaryExpr.Operator.EQUALS))
              }
            }
          }
      }
    def enable(): Generator[base.ProjectContext, Unit] = Enable.interpret(new Understands[base.ProjectContext, Enable.type] {
      def perform(
        context: ProjectCtxt,
        command: Enable.type
      ): (ProjectCtxt, Unit) = {
        val resolverUpdate =
          updateResolver(TypeRep.Boolean, Java("boolean").tpe())(new BooleanLiteralExpr(_))
        (context.copy(resolver = resolverUpdate(context.resolver)), ())
      }
    })
  }

  val equalityInMethod =
    new ContextIndependentEquality[MethodBodyCtxt](
      ooParadigm.methodBodyCapabilities.canGetMemberInMethod,
      paradigm.methodBodyCapabilities.canApplyInMethodBody
    )

  val equalityInConstructor =
    new ContextIndependentEquality[CtorCtxt](
      ooParadigm.constructorCapabilities.canGetMemberInConstructor,
      ooParadigm.constructorCapabilities.canApplyInConstructor
    )

  object assertionsInMethod extends Assertions[MethodBodyCtxt] {
    val base: paradigm.type = paradigm
    val assertionCapabilities: AssertionCapabilities =
      new AssertionCapabilities {
        implicit val canAssert: Understands[MethodBodyCtxt, Apply[Assert, base.syntax.Expression, base.syntax.Expression]] =
          new Understands[MethodBodyCtxt, Apply[Assert, base.syntax.Expression, base.syntax.Expression]] {
            def perform(
              context: MethodBodyCtxt,
              command: Apply[Assert, base.syntax.Expression, base.syntax.Expression]
            ): (MethodBodyCtxt, base.syntax.Expression) = {
              import ooParadigm.methodBodyCapabilities._
              import paradigm.methodBodyCapabilities._
              val assertImp = new ImportDeclaration("org.junit.Assert", false, false)
              val gen = for {
                _ <- addImport(assertImp)
                assertMethod <- getMember(Java("Assert").expression(), JavaNameProvider.mangle("assertTrue"))
                msg <- reify[String](TypeRep.String, command.functional.message)
                res <- apply(assertMethod, Seq(msg, command.arguments(0)))
              } yield res
              Command.runGenerator(gen, context)
            }
          }
      }

    override def enable(): Generator[base.ProjectContext, Unit] =
      Enable.interpret(new Understands[base.ProjectContext, Enable.type] {
        def perform(
          context: ProjectCtxt,
          command: Enable.type
        ): (ProjectCtxt, Unit) = {

          val resolverUpdate =
            updateResolver(TypeRep.Boolean, Java("boolean").tpe())(new BooleanLiteralExpr(_))
            .andThen(resolver =>
              resolver.copy(
                importResolution = {
                  case tpe if tpe == Java("org.junit.Assert").tpe() => Some(Java("org.junit.Assert").importDeclaration())
                  case other => context.resolver.importResolution(other)
                }
              )
            )

          (context.copy(resolver = resolverUpdate(context.resolver)), ())
        }
      })
  }
}

object CodeGenerator {

  sealed abstract class BoxLevel(val inMethods: Boolean, val inClasses: Boolean, val inConstructors: Boolean) {
    val isConsistent: Boolean =
      (inMethods == inClasses) && (inClasses == inConstructors)
  }
  case object FullyBoxed extends BoxLevel(inMethods = true, inClasses = true, inConstructors = true)
  case object PartiallyBoxed extends BoxLevel(inMethods = true, inClasses = false, inConstructors = false)
  case object Unboxed extends BoxLevel(inMethods = false, inClasses = false, inConstructors = false)

  case class Config(
    targetPackage: PackageDeclaration,
    projectName: Option[String],
    boxLevel: BoxLevel
  )

  val defaultConfig =
    Config(
      targetPackage = new PackageDeclaration(Java("ep").name),
      projectName = None,
      boxLevel = FullyBoxed
    )

  def apply(config: Config = defaultConfig): CodeGenerator =
    new CodeGenerator(config)
}
