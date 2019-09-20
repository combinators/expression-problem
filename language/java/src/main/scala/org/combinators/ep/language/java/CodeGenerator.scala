package org.combinators.ep.language.java

import java.nio.file.Paths

import com.github.javaparser.ast.`type`.TypeParameter
import com.github.javaparser.ast.{ImportDeclaration, Modifier, PackageDeclaration}
import com.github.javaparser.ast.body.{ClassOrInterfaceDeclaration, ConstructorDeclaration, MethodDeclaration}
import com.github.javaparser.ast.expr.AssignExpr.Operator
import com.github.javaparser.ast.expr.{AssignExpr, MethodCallExpr, NameExpr, VariableDeclarationExpr}
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
import org.combinators.templating.persistable.JavaPersistable

import scala.collection.JavaConverters._
import scala.util.Try
import cats.{ Apply => _, _}
import cats.implicits._
import cats.data.State
import org.combinators.ep.language.java.CodeGenerator.MarkUsed
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
    instantiationOverride: (Type, Seq[Expression]) => (Type, Seq[Expression])
  )

  case class ProjectCtxt(
    resolver: ContextSpecificResolver,
    units: Seq[com.github.javaparser.ast.CompilationUnit],
    extraDependencies: Seq[String]
  )
  case class CompilationUnitCtxt(
    resolver: ContextSpecificResolver,
    unit: State[FreshNameProvider[MangledName], com.github.javaparser.ast.CompilationUnit],
    freshNames: FreshNameProvider[MangledName],
    isTest: Boolean
  )
  case class ClassCtxt(
    resolver: ContextSpecificResolver,
    cls: State[FreshNameProvider[MangledName], ClassOrInterfaceDeclaration],
    freshNames: FreshNameProvider[MangledName],
    extraImports: Seq[Import]
  )
  case class TestCtxt(
    resolver: ContextSpecificResolver,
    freshNames: FreshNameProvider[MangledName],
    extraImports: Seq[Import],
    testClass: State[FreshNameProvider[MangledName], ClassOrInterfaceDeclaration]
  )
  case class MethodBodyCtxt(
    resolver: ContextSpecificResolver,
    freshNames: FreshNameProvider[MangledName],
    extraImports: Seq[Import],
    method: State[FreshNameProvider[MangledName], MethodDeclaration]
  )
  case class CtorCtxt(
    resolver: ContextSpecificResolver,
    freshNames: FreshNameProvider[MangledName],
    extraImports: Seq[Import],
    ctor: State[FreshNameProvider[MangledName], ConstructorDeclaration]
  )
  case class TypeParamCtxt(
    param: TypeParameter
  )

  private def pushName(name: MangledName, useCounter: Int): MangledName = {
    if (useCounter == 0) {
      name
    } else if (useCounter == 1) {
      MangledNameProvider.addPrefix("_", name)
    } else {
      MangledNameProvider.addSuffix(name, useCounter.toString)
    }
  }

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
              val (uc, _) =
                Command.runGenerator(
                  command.unit,
                  CompilationUnitCtxt(
                    context.resolver,
                    State.pure(new com.github.javaparser.ast.CompilationUnit()),
                    FreshNameProvider[MangledName](pushName),
                    isTest = false)
                )
              val (_, resultUnit) = uc.unit.run(uc.freshNames).value
              (context.copy(resolver = uc.resolver, units = context.units :+ resultUnit), ())
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
              val freshNames = context.freshNames.markUsed(MangledNameProvider.mangle(command.imp.getName.getIdentifier))
              val updatedUnit: State[FreshNameProvider[MangledName], CompilationUnit] =
                for {
                  oldUnit <- context.unit
                  newUnit = if (!oldUnit.getImports.contains(command.imp)) {
                    val nextUnit = oldUnit.clone()
                    nextUnit.addImport(command.imp)
                    nextUnit
                  } else oldUnit
                } yield newUnit
              (context.copy(unit = updatedUnit, freshNames = freshNames), ())
            }
          }
        implicit val canAddTestSuiteInCompilationUnit: Understands[CompilationUnitCtxt, AddTestSuite[Name, TestCtxt]] =
          new Understands[CompilationUnitCtxt, AddTestSuite[Name, TestCtxt]] {
            def perform(
              context: CompilationUnitCtxt,
              command: AddTestSuite[Name, TestCtxt]
            ): (CompilationUnitContext, Unit) = {
              val (freshNames, testName) = command.name.run(context.freshNames).value
               val (testRes, _) =
                Command.runGenerator(
                  command.suite,
                  TestCtxt(
                    context.resolver,
                    freshNames.markUsed(testName),
                    Seq.empty,
                    State.pure(new ClassOrInterfaceDeclaration())))
              val updatedUnit =
                for {
                  oldUnit <- context.unit
                  testClass <- testRes.testClass
                  newUnit = {
                    val nextUnit = oldUnit.clone()
                    testRes.extraImports.foreach { imp =>
                      if (!nextUnit.getImports.contains(imp))
                        nextUnit.addImport(imp)
                    }
                    val classToAdd = testClass.clone()
                    classToAdd.setName(testName.mangled)
                    nextUnit.addType(classToAdd)
                    nextUnit
                  }
                } yield newUnit
              (context.copy(resolver = testRes.resolver, freshNames = testRes.freshNames, unit = updatedUnit, isTest = true), ())
            }
          }

        implicit val canGetFreshNameInCompilationUnit: Understands[CompilationUnitContext, FreshName[Name]] =
          new Understands[CompilationUnitContext, FreshName[Name]] {
            def perform(context: CompilationUnitContext, command: FreshName[Name]): (CompilationUnitContext, Name) = {
              (context,
                for {
                  basedOn <- command.basedOn
                  nameGen <- State.get[FreshNameProvider[MangledName]]
                  (name, next) = nameGen.freshNameBasedOn(basedOn)
                  _ <- State.set(next)
                } yield name)
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
              val freshNames = extraImports.foldLeft(context.freshNames) { case (s, imp) =>
                s.markUsed(MangledNameProvider.mangle(imp.getName.getIdentifier))
              }
              (context.copy(extraImports = extraImports, freshNames = freshNames), ())
            }
          }
        implicit val canAddBlockDefinitionsInMethodBody: Understands[MethodBodyCtxt, AddBlockDefinitions[Statement]] =
          new Understands[MethodBodyCtxt, AddBlockDefinitions[Statement]] {
            def perform(
              context: MethodBodyCtxt,
              command: AddBlockDefinitions[Statement]
            ): (MethodBodyCtxt, Unit) = {
              val updatedMethod =
                for {
                  oldMethod <- context.method
                  stmts <- command.definitions.toList.sequence
                  newMethod = {
                    val nextMethod = oldMethod.clone()
                    val body = nextMethod.getBody.orElseGet(() => new BlockStmt())
                    stmts.foreach(stmt => body.addStatement(stmt.clone()))
                    nextMethod.setBody(body)
                    nextMethod
                  }
                } yield newMethod
              (context.copy(method = updatedMethod), ())
            }
          }

        implicit val canSetReturnTypeInMethodBody: Understands[MethodBodyCtxt, SetReturnType[Type]] =
          new Understands[MethodBodyCtxt, SetReturnType[Type]] {
            def perform(
              context: MethodBodyCtxt,
              command: SetReturnType[Type]
            ): (MethodBodyCtxt, Unit) = {
              val updatedMethod = for {
                oldMethod <- context.method
                tpe <- command.tpe
                newMethod = {
                  val nextMethod = oldMethod.clone()
                  nextMethod.setType(tpe.clone())
                  nextMethod
                }
              } yield newMethod
              (context.copy(method = updatedMethod), ())
            }
          }

        implicit val canSetParametersInMethodBody: Understands[MethodBodyCtxt, SetParameters[Name, Type]] =
          new Understands[MethodBodyCtxt, SetParameters[Name, Type]] {
            def perform(
              context: MethodBodyCtxt,
              command: SetParameters[Name, Type]
            ): (MethodBodyCtxt, Unit) = {
              val updatedMethod = for {
                oldMethod <- context.method
                _ <- State.modify { freeNames: FreshNameProvider[MangledName] =>
                  oldMethod.getParameters.asScala.foldLeft(freeNames) { (nextFree, param) =>
                    nextFree.markUnused(MangledNameProvider.mangle(param.getNameAsString))
                  }
                }
                params <-
                  command
                    .params
                    .map { case (name, tpe) =>
                      for {
                        n <- name
                        _ <- State.modify { freeNames: FreshNameProvider[MangledName] => freeNames.markUsed(n) }
                        t <- tpe
                      } yield (n, t)
                    }
                    .toList
                    .sequence
                newMethod = {
                  val nextMethod = oldMethod.clone()
                  nextMethod.getParameters.clear()
                  params.foreach { case (paramName, paramTpe) =>
                    nextMethod.addParameter(paramTpe.clone(), paramName.mangled)
                  }
                  nextMethod
                }
              } yield newMethod
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
                val (freshNames, importTpe) = command.forElem.run(context.freshNames).value
                val importName = importTpe.asClassOrInterfaceType().getNameAsString
                val newImport =
                  new ImportDeclaration(
                    new com.github.javaparser.ast.expr.Name(config.targetPackage.getName.clone(), importName),
                    false,
                    false)
                if (context.extraImports.contains(newImport)) {
                  (context, None)
                } else {
                  (context.copy(freshNames = freshNames), Some(newImport))
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
                for {
                  functional <- command.functional
                  args <- command.arguments.toList.sequence
                } yield Java(s"${functional}(${args.mkString(", ")})").expression()
              (context, resultExp)
            }
          }

        implicit val canGetArgumentsInMethodBody: Understands[MethodBodyCtxt, GetArguments[Type, Name, Expression]] =
          new Understands[MethodBodyCtxt, GetArguments[Type, Name, Expression]] {
            def perform(
              context: MethodBodyCtxt,
              command: GetArguments[Type, Name, Expression]
            ): (MethodBodyCtxt, Seq[(Name, Type, Expression)]) = {
              val argIter = new Iterator {
                var idx = 0
                var nextElem = Try {
                  val name = context.method.map {
                    method.param
                  }
                }
                def hasNext: Boolean = {

                }

                def next(): A = ???
              }
              val names = context.method.map { method =>
                method.getParameters.asScala.map(param => MangledNameProvider.mangle(param.getName.toString))
              }
              val types = context.method.map { method =>
                method.getParameters.asScala.map(param => param.getType.clone())
              }
              val exps = context.method.map { method =>
                method.getParameters.asScala.map(param => new NameExpr(param.getName.clone()))
              }
              (context, names.zip(types).zip(exps))
            }
          }
      }
      val testCapabilities: TestCapabilities =
        new TestCapabilities {
          implicit val canAddTestCaseInTest: Understands[TestContext, AddTestCase[MethodBodyContext, Expression]] =
            new Understands[TestContext, AddTestCase[MethodBodyContext, Expression]] {
              def perform(
                context: TestContext,
                command: AddTestCase[MethodBodyContext, Expression]
              ): (TestContext, Unit) = {
                val gen: Generator[MethodBodyCtxt, Unit] = {
                  import methodBodyCapabilities._
                  for {
                    assertions <- command.code
                    _ <- addBlockDefinitions(assertions.map(exp => new ExpressionStmt(exp.clone())))
                  } yield ()
                }
                val testMethod = new MethodDeclaration(
                  Modifier.PUBLIC.toEnumSet,
                  new com.github.javaparser.ast.`type`.VoidType(),
                  "test" + command.name.capitalize)
                testMethod.addAnnotation("@Test")
                val (resultingContext, _) =
                  Command.runGenerator(
                    gen,
                    MethodBodyCtxt(
                      context.resolver,
                      new VariableNameProvider(),
                      context.extraImports,
                      testMethod)
                  )
                val newClass = context.testClass.clone()
                newClass.addMember(resultingContext.method)
                (context.copy(resolver = resultingContext.resolver, testClass = newClass), ())
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
                instantiationOverride = (tpe, args) => (tpe, args)
              ),
              units = Seq.empty,
              extraDependencies = Seq.empty
            )
          )
        val nameEntry = config.projectName.map(n => s"""name := "${n}"""").getOrElse("")
        val deps = finalContext.extraDependencies.mkString("Seq(\n", ",\n    ", "\n  )")
        val buildFile =
          s"""
             |$nameEntry
             |libraryDependencies ++= $deps
           """.stripMargin
        val javaFiles = finalContext.units.map { unit =>
          FileWithPath(
            JavaPersistable.compilationUnitInstance.rawText(unit),
            JavaPersistable.compilationUnitInstance.fullPath(Paths.get("src", "main", "java"), unit)
          )
        }
        FileWithPath(buildFile, Paths.get("build.sbt")) +: javaFiles
      }
    }

  /*object ooParadigm extends ObjectOriented {
      val base: paradigm.type = paradigm
      type ClassContext = ClassCtxt
      type ConstructorContext = CtorCtxt
      val compilationUnitCapabilities: CompilationUnitCapabilities =
        new CompilationUnitCapabilities {
          implicit val canAddClassInCompilationUnit: Understands[base.CompilationUnitContext, AddClass[ClassContext]] =
            new Understands[base.CompilationUnitContext, AddClass[ClassContext]] {
              def perform(
                context: base.CompilationUnitContext,
                command: AddClass[ClassContext]
              ): (base.CompilationUnitContext, Unit) = {
                val clsToAdd = new ClassOrInterfaceDeclaration()
                clsToAdd.setName(command.name)
                clsToAdd.setPublic(true)
                val (resultCtxt, _) =
                  Command.runGenerator(
                    command.cls,
                    ClassCtxt(context.resolver, context.unit.getImports().asScala, clsToAdd)
                  )
                val newUnit = context.unit.clone()
                newUnit.addType(resultCtxt.cls)
                resultCtxt.extraImports.foreach { imp =>
                  if (!newUnit.getImports.contains(imp)) {
                    newUnit.addImport(imp.clone())
                  }
                }
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
                resultCls.addExtendedType(command.parentClass.toString())
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
                resultCls.addImplementedType(command.interface.toString())
                (context.copy(cls = resultCls), ())
              }
            }
          implicit val canAddFieldInClass: Understands[ClassContext, AddField[Type]] =
            new Understands[ClassContext, AddField[Type]] {
              def perform(
                context: ClassContext,
                command: AddField[Type]
              ): (ClassContext, Unit) = {
                val resultCls = context.cls.clone()
                resultCls.addField(command.tpe, command.name, Modifier.PRIVATE)
                (context.copy(cls = resultCls), ())
              }
            }
          implicit val canAddMethodInClass: Understands[ClassContext, AddMethod[base.MethodBodyContext, Option[Expression]]] =
            new Understands[ClassContext, AddMethod[base.MethodBodyContext, Option[Expression]]] {
              def perform(
                context: ClassContext,
                command: AddMethod[base.MethodBodyContext, Option[Expression]]
              ): (ClassContext, Unit) = {
                val methodToAdd = new MethodDeclaration()
                methodToAdd.setName(command.name)
                methodToAdd.setPublic(command.isPublic)
                val (methodCtxt, returnExp) =
                  Command.runGenerator(
                    command.spec,
                    MethodBodyCtxt(context.resolver, new VariableNameProvider(), context.extraImports, methodToAdd)
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
                val (ctorCtxt, _) =
                  Command.runGenerator(
                    command.ctor,
                    CtorCtxt(context.resolver, new VariableNameProvider(), context.extraImports, ctorToAdd)
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
                (context.copy(extraImports = (context.extraImports :+ command.imp).distinct.map(_.clone())), ())
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
                      new Name(config.targetPackage.getName.clone(), command.forElem.toString()),
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
          implicit val canFindClassInClass: Understands[ClassContext, FindClass[Type]] =
            new Understands[ClassContext, FindClass[Type]] {
              def perform(
                context: ClassContext,
                command: FindClass[Type]
              ): (ClassContext, Type) = {
                (context, Java(command.name).tpe())
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
          implicit val canInitializeFieldInConstructor: Understands[ConstructorContext, InitializeField[Expression]] =
            new Understands[ConstructorContext, InitializeField[Expression]] {
              def perform(
                context: ConstructorContext,
                command: InitializeField[Expression]
              ): (ConstructorContext, Unit) = {
                Command.runGenerator(
                  addBlockDefinitions(Java(s"this.${command.name} = ${command.value}").statements()),
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
                      new Name(config.targetPackage.getName.clone(), command.forElem.toString()),
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

          implicit val canGetMemberInConstructor: Understands[ConstructorContext, GetMember[Expression]] =
            new Understands[ConstructorContext, GetMember[Expression]] {
              def perform(
                context: ConstructorContext,
                command: GetMember[Expression]
              ): (ConstructorContext, Expression) = {
                (context, Java(s"""this.${command.member}""").expression())
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

          implicit val canGetArgumentsInConstructor: Understands[ConstructorContext, GetArguments[Type, Expression]] =
            new Understands[ConstructorContext, GetArguments[Type, Expression]] {
              def perform(
                context: ConstructorContext,
                command: GetArguments[Type, Expression]
              ): (ConstructorContext, Seq[(String, Type, Expression)]) = {
                val args = context.ctor.getParameters().asScala.map { param =>
                  (param.getName.toString(), param.getType().clone(), new NameExpr(param.getName.clone()))
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
          implicit val canSetParametersInConstructor: Understands[ConstructorContext, SetParameters[Type]] =
            new Understands[ConstructorContext, SetParameters[Type]] {
              def perform(
                context: ConstructorContext,
                command: SetParameters[Type]
              ): (ConstructorContext, Unit) = {
                val newCtor = context.ctor.clone()
                val clearedNames = newCtor.getParameters.asScala.foldLeft(context.freshNames) { case (names, param) =>
                    names.freeupName(param.getNameAsString)
                }
                newCtor.getParameters.clear()
                val freshNames = command.params.foldLeft(clearedNames) { case (names, (name, tpe)) =>
                  val (paramName, nextNames) = names.useName(name)
                  newCtor.addParameter(tpe, paramName)
                  nextNames
                }
                (context.copy(ctor = newCtor, freshNames = freshNames), ()) // second thing to be returned isn't optional, so make it () is like Unit
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
          implicit val canFindClassInConstructor: Understands[ConstructorContext, FindClass[Type]] =
            new Understands[ConstructorContext, FindClass[Type]] {
              def perform(
                context: ConstructorContext,
                command: FindClass[Type]
              ): (ConstructorContext, Type) = {
                (context, Java(command.name).tpe())
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
          val canGetMemberInMethod: Understands[MethodBodyContext, GetMember[Expression]] =
            new Understands[MethodBodyContext, GetMember[Expression]] {
              def perform(
                context: MethodBodyContext,
                command: GetMember[Expression]
              ): (MethodBodyContext, Expression) = {
                (context, Java(s"""this.${command.member}""").expression())
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
          val canFindClassInMethod: Understands[MethodBodyContext, FindClass[Type]] =
            new Understands[MethodBodyContext, FindClass[Type]] {
              def perform(
                context: MethodBodyContext,
                command: FindClass[Type]
              ): (MethodBodyContext, Type) = {
                (context, Java(command.name).tpe())
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
    def getVariableNameProvider(ctxt: Ctxt): VariableNameProvider
    def copyWithVariableNameProvider(ctxt: Ctxt, freshNames: VariableNameProvider): Ctxt
  }

  protected class ContextIndependentImperativeCapabilities[Ctxt](manip: BlockContextManipulator[Ctxt]) extends Imperative[Ctxt] {
    val base: paradigm.type = paradigm
    object imperativeCapabilities extends ImperativeCapabilities {
      implicit val canDeclareFreshVariable: Understands[Ctxt, DeclareVariable[Type, Option[Expression], Expression]] =
        new Understands[Ctxt, DeclareVariable[Type, Option[Expression], Expression]] {
          def perform(context: Ctxt, command: DeclareVariable[Type, Option[Expression], Expression]): (Ctxt, Expression) = {
            val (vname, nextFresh) = manip.getVariableNameProvider(context).useName(command.name)
            val decl = new VariableDeclarationExpr(command.tpe, vname)
            val withAssignment =
              if (command.initialization.isDefined) {
                new AssignExpr(decl, command.initialization.get.clone(), Operator.ASSIGN)
              } else {
                decl
              }
            val nextBlock = manip.getBlock(context).clone()
            nextBlock.addStatement(withAssignment)
            (manip.copyWithBlock(manip.copyWithVariableNameProvider(context, nextFresh), nextBlock), new NameExpr(vname))
          }
        }

      implicit val canAssignVariable: Understands[Ctxt, AssignVariable[Expression, Statement]] =
        new Understands[Ctxt, AssignVariable[Expression, Statement]] {
          def perform(context: Ctxt, command: AssignVariable[Expression, Statement]): (Ctxt, Statement) = {
            (context, new ExpressionStmt(new AssignExpr(command.variable.clone(), command.value.clone(), Operator.ASSIGN)))
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
            val namesBeforeIf = manip.getVariableNameProvider(context)
            val (startCtxt, _) = Command.runGenerator(command.ifBranch, manip.nextBlockContext(context))
            val iteStmt: IfStmt = new IfStmt()
            iteStmt.setCondition(command.condition)
            iteStmt.setThenStmt(manip.getBlock(startCtxt))
            val (beforeElseCtxt, beforeElseIteStmt) =
              command.elseIfBranches.foldLeft((manip.copyWithVariableNameProvider(manip.nextBlockContext(startCtxt), namesBeforeIf), iteStmt)) {
                case ((ctxt, iteStmt), (cond, gen)) =>
                  val (nextCtxt, _) = Command.runGenerator(gen, manip.nextBlockContext(ctxt))
                  val nextIte = new IfStmt()
                  nextIte.setCondition(cond)
                  nextIte.setThenStmt(manip.getBlock(nextCtxt))
                  iteStmt.setElseStmt(nextIte)
                  (manip.copyWithVariableNameProvider(nextCtxt, namesBeforeIf), nextIte)
              }
            val afterElseCtxt =
              command.elseBranch
                .map { elseGen =>
                  val (resCtxt, _) = Command.runGenerator(elseGen, manip.nextBlockContext(beforeElseCtxt))
                  beforeElseIteStmt.setElseStmt(manip.getBlock(resCtxt))
                  resCtxt
                }.getOrElse(beforeElseCtxt)
            (manip.copyWithVariableNameProvider(manip.copyWithBlock(afterElseCtxt, manip.getBlock(context)), namesBeforeIf), iteStmt.clone())
          }
        }
      implicit val canWhile: Understands[Ctxt, While[Ctxt, Expression, Statement]] =
        new Understands[Ctxt, While[Ctxt, Expression, Statement]] {
          def perform(context: Ctxt, command: While[Ctxt, Expression, Statement]): (Ctxt, Statement) = {
            val namesBeforeIf = manip.getVariableNameProvider(context)
            val (whileCtxt, _) = Command.runGenerator(command.block, manip.nextBlockContext(context))
            val whileStmt = new WhileStmt()
            whileStmt.setCondition(command.condition)
            whileStmt.setBody(manip.getBlock(whileCtxt))
            (manip.copyWithVariableNameProvider(manip.copyWithBlock(whileCtxt, manip.getBlock(context)), namesBeforeIf), whileStmt)
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
      def getVariableNameProvider(ctxt: MethodBodyCtxt): VariableNameProvider = ctxt.freshNames
      def copyWithVariableNameProvider(ctxt: MethodBodyCtxt, freshNames: VariableNameProvider): MethodBodyCtxt =
        ctxt.copy(freshNames = freshNames)
    }
  )

  val imperativeInConstructor = new ContextIndependentImperativeCapabilities[CtorCtxt](
    new BlockContextManipulator[CtorCtxt] {
      def getBlock(ctxt: CtorCtxt): BlockStmt = ctxt.ctor.getBody()
      def copyWithBlock(ctxt: CtorCtxt, blockStmt: BlockStmt): CtorCtxt = {
        val newCtor = ctxt.ctor.clone()
        newCtor.setBody(blockStmt.clone())
        ctxt.copy(ctor = newCtor)
      }
      def getVariableNameProvider(ctxt: CtorCtxt): VariableNameProvider = ctxt.freshNames
      def copyWithVariableNameProvider(ctxt: CtorCtxt, freshNames: VariableNameProvider): CtorCtxt =
        ctxt.copy(freshNames = freshNames)
    }
  )

  object parametricPolymorphism extends ParametricPolymorphism {
    val base: paradigm.type = paradigm
    type TypeParameterContext = TypeParamCtxt
    import base._
    val methodBodyCapabilities: MethodBodyCapabilities =
      new MethodBodyCapabilities {
        implicit val canAddTypeParameterInMethod: Understands[MethodBodyContext, AddTypeParameter[TypeParameterContext]] = ???
        implicit val canGetTypeArgumentsInMethod: Understands[MethodBodyContext, GetTypeArguments[Type]] = ???
        implicit val canApplyTypeInMethod: Understands[MethodBodyContext, Apply[Type, Type, Type]] = ???
        implicit val canApplyMethodToTypeInMethod: Understands[MethodBodyContext, Apply[Expression, Type, Expression]] = ???
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
        implicit val canAddTypeParameterInClass: Understands[ClassContext, AddTypeParameter[TypeParameterContext]] =
          new Understands[ClassContext, AddTypeParameter[TypeParameterContext]] {
            def perform(context: ClassContext, command: AddTypeParameter[TypeParameterContext]): (ClassContext, Unit) = {

            }
          }
        implicit val canGetTypeArgumentsInClass: Understands[ClassContext, GetTypeArguments[Type]] = ???
        implicit val canApplyTypeInClass: Understands[ClassContext, Apply[Type, Type, Type]] =
          new Understands[ClassContext, Apply[Type, Type, Type]] {
            def perform(context: ClassContext, command: Apply[Type, Type, Type]): (ClassContext, Type) = {
              (context, Java(s"${command.functional}<${command.arguments.mkString(",")}>").tpe())
            }
          }
      }
    val typeParameterCapabilities: TypeParameterCapabilities =
      new TypeParameterCapabilities {
        implicit val canAddUpperBoundInTypeParameter: Understands[TypeParameterContext, AddUpperBound[Type]] =
          new Understands[TypeParameterContext, AddUpperBound[Type]] {
            def perform(context: TypeParameterContext, command: AddUpperBound[Type]): (TypeParameterContext, Unit) = {
              val newParam = context.param.clone()
              newParam.getTypeBound.add(command.bound.asClassOrInterfaceType().clone())
              (context.copy(param = newParam), ())
            }
          }
        implicit val canAddLowerBoundInTypeParameter: Understands[TypeParameterContext, AddLowerBound[Type]] =
          throw new UnsupportedOperationException("Sorry, Java does not support lower bounds on type parameters.")
        implicit val canApplyTypeTypeParameter: Understands[TypeParameterContext, Apply[Type, Type, Type]] =
          new Understands[TypeParameterContext, Apply[Type, Type, Type]] {
            def perform(context: TypeParameterContext, command: Apply[Type, Type, Type]): (TypeParameterContext, Type) = {
              (context, Java(s"${command.functional}<${command.arguments.mkString(",")}>").tpe())
            }
          }
      }
    val constructorCapabilities: ConstructorCapabilities =
      new ConstructorCapabilities {
        implicit val canApplyTypeInConstructor: Understands[ConstructorContext, Apply[Type, Type, Type]] = ???
        implicit val canApplyMethodToTypeInConstructor: Understands[ConstructorContext, Apply[Expression, Type, Expression]] = ???
      }
  }
*/

  object javaSpecificCompilationUnitCapabilities {
    implicit val canMarkUsedInCompulationUnit: Understands[CompilationUnitCtxt, MarkUsed] =
      new Understands[CompilationUnitCtxt, MarkUsed] {
        def perform(
          context: CompilationUnitCtxt,
          command: MarkUsed
        ): (CompilationUnitCtxt, Unit) = {
          val (next, name) = command.name.run(context.freshNames).value
          (context.copy(freshNames = next.markUsed(name)), ())
        }
      }
    def markUsed(name: Name): Generator[CompilationUnitCtxt, Unit] =
      AnyParadigm.capabilitiy(MarkUsed(name))
  }

}

object CodeGenerator {

  case class MarkUsed(name: Syntax.default.Name) extends Command {
    type Result = Unit
  }


  case class Config(
    targetPackage: PackageDeclaration,
    projectName: Option[String]
  )

  val defaultConfig =
    Config(
      targetPackage = new PackageDeclaration(),
      projectName = None
    )

  def apply(config: Config = defaultConfig): CodeGenerator =
    new CodeGenerator(config)
}
