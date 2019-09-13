package org.combinators.ep.language.java

import java.nio.file.Paths

import com.github.javaparser.ast.`type`.Type
import com.github.javaparser.ast.{ImportDeclaration, Modifier, PackageDeclaration}
import com.github.javaparser.ast.body.{ClassOrInterfaceDeclaration, ConstructorDeclaration, MethodDeclaration}
import com.github.javaparser.ast.expr.{MethodCallExpr, Name, NameExpr}
import com.github.javaparser.ast.stmt.{BlockStmt, ExplicitConstructorInvocationStmt, ExpressionStmt}
import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{AbstractSyntax, Command, FileWithPath, Understands}
import org.combinators.ep.generator.paradigm.{AddBlockDefinitions, AddClass, AddCompilationUnit, AddConstructor, AddField, AddImplemented, AddImport, AddMethod, AddParent, AddTestCase, AddTestSuite, AddTypeLookup, AnyParadigm, Apply, FindClass, GetArguments, GetConstructor, GetMember, InitializeField, InitializeParent, InstantiateObject, ObjectOriented, Reify, ResolveImport, SelfReference, SetAbstract, SetInterface, SetParameters, SetReturnType, ToTargetLanguageType}
import org.combinators.templating.twirl.Java
import org.combinators.templating.persistable.JavaPersistable

import scala.collection.JavaConverters._
import scala.util.Try

/**
 * Java-specific.
 *
 * These paradigm-specific traits are conceptually different from each other
 */
sealed class CodeGenerator(config: CodeGenerator.Config) {
  import Syntax.default._

  case class ContextSpecificResolver(
    methodTypeResulution: TypeRep => Generator[MethodBodyCtxt, Type],
    constructorTypeResolution: TypeRep => Generator[CtorCtxt, Type],
    classTypeResolution: TypeRep => Generator[ClassCtxt, Type],
    reificationInConstructor: InstanceRep => Generator[CtorCtxt, Expression],
    reificationInMethod: InstanceRep => Generator[MethodBodyCtxt, Expression],
    importResolution: Type => Option[Import],
    instantiationOverride: (Type, Seq[Expression]) => (Type, Seq[Expression])
  )

  case class ProjectCtxt(
    resolver: ContextSpecificResolver,
    units: Seq[CompilationUnit],
    extraDependencies: Seq[String]
  )
  case class CompilationUnitCtxt(
    resolver: ContextSpecificResolver,
    unit: CompilationUnit,
    isTest: Boolean
  )
  case class ClassCtxt(
    resolver: ContextSpecificResolver,
    extraImports: Seq[Import],
    cls: ClassOrInterfaceDeclaration
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

  object paradigm extends AnyParadigm {
      val syntax: Syntax.default.type = Syntax.default
      type ProjectContext = ProjectCtxt
      type CompilationUnitContext = CompilationUnitCtxt
      type TestContext = TestCtxt
      type MethodBodyContext = MethodBodyCtxt

      val projectContextCapabilities: ProjectContextCapabilities =
        new ProjectContextCapabilities {
          implicit val canAddCompilationUnitInProject: Understands[ProjectCtxt, AddCompilationUnit[CompilationUnitCtxt]] =
            new Understands[ProjectCtxt, AddCompilationUnit[CompilationUnitCtxt]] {
              def perform(
                context: ProjectCtxt,
                command: AddCompilationUnit[CompilationUnitCtxt]
              ): (ProjectCtxt, Unit) = {
                val (uc, _) =
                  Command.runGenerator(command.unit,
                    CompilationUnitCtxt(context.resolver, new com.github.javaparser.ast.CompilationUnit(), false))
                (context.copy(resolver = uc.resolver, units = context.units :+ uc.unit), ())
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
                    context.resolver.methodTypeResulution(tpe)
                  }
                (context.copy(resolver = context.resolver.copy(methodTypeResulution = newLookup)), ())
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
                val (testRes, _) =
                  Command.runGenerator(
                    command.suite,
                    TestCtxt(context.resolver, Seq.empty, new ClassOrInterfaceDeclaration()))
                testRes.extraImports.foreach { imp =>
                  if (!newUnit.getImports.contains(imp)) {
                    newUnit.addImport(imp)
                  }
                }
                testRes.testClass.setName(command.name)
                newUnit.addType(testRes.testClass)
                (context.copy(resolver = testRes.resolver, unit = newUnit, isTest = true), ())
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
                (context.copy(extraImports = (context.extraImports :+ command.imp).distinct.map(_.clone())), ())
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
                (context.copy(method = newMethod), ()) // second thing to be returned isn't optional, so make it () is like Unit
              }
            }
          implicit val canTransformTypeInMethodBody: Understands[MethodBodyCtxt, ToTargetLanguageType[Type]] =
            new Understands[MethodBodyContext, ToTargetLanguageType[Type]] {
              def perform(
                context: MethodBodyContext,
                command: ToTargetLanguageType[Type]
              ): (MethodBodyContext, Type) = {
                Command.runGenerator(context.resolver.methodTypeResulution(command.tpe), context)
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

          implicit val canApplyInMethodBody: Understands[MethodBodyCtxt, Apply[Expression, Expression, Expression]] =
            new Understands[MethodBodyCtxt, Apply[Expression, Expression, Expression]] {
              def perform(
                context: MethodBodyCtxt,
                command: Apply[Expression, Expression, Expression]
              ): (MethodBodyCtxt, Expression) = {
                (context, Java(s"${command.functional}(${command.arguments.mkString(", ")})").expression())
              }
            }

          implicit val canGetArgumentsInMethodBody: Understands[MethodBodyCtxt, GetArguments[Type, Expression]] =
            new Understands[MethodBodyCtxt, GetArguments[Type, Expression]] {
              def perform(
                context: MethodBodyCtxt,
                command: GetArguments[Type, Expression]
              ): (MethodBodyCtxt, Seq[(String, Type, Expression)]) = {
                val args = context.method.getParameters().asScala.map { param =>
                  (param.getName.toString(), param.getType().clone(), new NameExpr(param.getName.clone()))
                }
                (context, args)
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
                methodTypeResulution = _ => ???,
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

  object ooParadigm extends ObjectOriented {
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
                val (methodCtxt, _) =
                  Command.runGenerator(command.spec, MethodBodyCtxt(context.resolver, context.extraImports, methodToAdd))
                val resultCls = context.cls.clone()
                resultCls.addMember(methodCtxt.method.clone())
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
                  Command.runGenerator(command.ctor, CtorCtxt(context.resolver, context.extraImports, ctorToAdd))
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
                newCtor.getParameters.clear()
                command.params.foreach { case (name, tpe) =>
                  newCtor.addParameter(tpe, name)
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
          implicit val canAddTypeLookupForClassesInProject: Understands[ProjectContext, AddTypeLookup[ClassContext, Type]] = ???
          implicit val canAddTypeLookupForConstructorsInProject: Understands[ProjectContext, AddTypeLookup[ConstructorContext, Type]] = ???
        }
    }
}

object CodeGenerator {
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
