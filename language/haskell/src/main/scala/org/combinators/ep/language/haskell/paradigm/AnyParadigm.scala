package org.combinators.ep.language.haskell.paradigm    /*DI:LD:AI*/

import java.nio.file.Paths
import java.util.UUID
import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{Command, FileWithPath, FreshNameProvider, Understands}
import org.combinators.ep.generator.paradigm.{AnyParadigm => AP, _}
import org.combinators.ep.language.haskell.Syntax.MangledName
import org.combinators.ep.language.haskell.ast
import org.combinators.ep.language.haskell.ast.CompilationUnit
import org.combinators.ep.language.haskell.{CompilationUnitCtxt, MethodBodyCtxt, ProjectCtxt, Syntax, TestCtxt, ContextSpecificResolver}
//import org.combinators.templating.persistable.{BundledResource, JavaPersistable}
//import org.combinators.ep.language.java.ResourcePersistable

trait AnyParadigm extends AP {
  //val config: Config
  val syntax: Syntax.default.type = Syntax.default
  import syntax._

  type ProjectContext = ProjectCtxt
  type CompilationUnitContext = CompilationUnitCtxt
  type TestContext = TestCtxt
  type MethodBodyContext = MethodBodyCtxt


  val projectCapabilities: ProjectCapabilities = new ProjectCapabilities {
    implicit val canDebugInProject: Understands[ProjectContext,Debug] = new Understands[ProjectCtxt, Debug] {
      def perform(context: ProjectCtxt, command: Debug): (ProjectCtxt, Unit) = {
        context.units.foreach (u => System.err.println (command.tag + ": " + u))
        (context,())
      }
    }
    
    implicit val canAddCompilationUnitInProject: Understands[ProjectContext,AddCompilationUnit[Name,CompilationUnitContext]] = new Understands[ProjectContext, AddCompilationUnit[Name,CompilationUnitContext]] {
      def perform(context: ProjectContext, command: AddCompilationUnit[Name,CompilationUnitContext]): (ProjectContext, Unit) = {
        val (uc, _) = Command.runGenerator(command.unit, CompilationUnitCtxt(
          resolver = context.resolver,
          freshNameProvider = FreshNameProvider[Name]((n, s) => n.copy(n.original + s)),
          unit = CompilationUnit(
            name = command.name.toAST,
            imports = Seq.empty,
            typeDecls = Seq.empty,
            typeClassDecls = Seq.empty,
            typeClassInstances = Seq.empty,
            funDecls = Seq.empty
          ),
          isTest = false
        ))
        (context.copy(resolver = uc.resolver, units = context.units :+ uc.unit), ())
      }
    }
    
    implicit val canAddTypeLookupForMethodsInProject: Understands[ProjectContext,AddTypeLookup[MethodBodyContext,Type]] =
      new Understands[ProjectContext,AddTypeLookup[MethodBodyContext,Type]] {
        def perform(context: ProjectContext, command: AddTypeLookup[MethodBodyContext,Type]): (ProjectContext, Unit) = {
          def newLookup(k: ContextSpecificResolver)(tpe: TypeRep): Generator[MethodBodyCtxt, Type] = {
              if (tpe == command.tpe) {
                command.lookup
              } else {
                context.resolver._methodTypeResolution(k)(tpe)
              }
            }
          (context.copy(resolver = context.resolver.copy(_methodTypeResolution = newLookup)), ())
        }
      }
  }

  val compilationUnitCapabilities: CompilationUnitCapabilities =
    new CompilationUnitCapabilities {
     implicit val canDebugInCompilationUnit: Understands[CompilationUnitContext, Debug] =
       new Understands[CompilationUnitContext, Debug] {
        def perform(context: CompilationUnitContext, command: Debug): (CompilationUnitContext, Unit) = {
          System.err.println (command.tag + ": " + context.unit)
          (context,())
        }
      }
     
     implicit val canAddImportInCompilationUnit: Understands[CompilationUnitContext,AddImport[Import]] = 
       new Understands[CompilationUnitContext,AddImport[Import]] {
        def perform(context: CompilationUnitContext, command: AddImport[Import]): (CompilationUnitContext, Unit) = {
          val imports = (context.unit.imports :+ command.imp).distinct
          (context.copy(unit = context.unit.copy(imports = imports)), ())
        }
       }
     
     implicit val canAddTestSuiteInCompilationUnit: Understands[CompilationUnitContext,AddTestSuite[Name,TestContext]] = 
        new Understands[CompilationUnitContext,AddTestSuite[Name,TestContext]] {
          def perform(context: CompilationUnitContext, command: AddTestSuite[Name,TestContext]): (CompilationUnitContext, Unit) = {
            val (uc, _) = Command.runGenerator(command.suite, TestCtxt(
              resolver = context.resolver,
              freshNameProvider = FreshNameProvider[Name]((n, s) => n.copy(n.original + s)),
              extraImports = Seq.empty
              ))
            Command.runGenerator(debug("FIXME: do something for tests in Haskell"), context)
            (context, ())
          }
        }
     
     implicit val canGetFreshNameInCompilationUnit: Understands[CompilationUnitContext,FreshName[Name]] = 
       new Understands[CompilationUnitContext,FreshName[Name]] {
         def perform(context: CompilationUnitContext, command: FreshName[Name]): (CompilationUnitContext, Name) = {
            val (name, np) = context.freshNameProvider.freshNameBasedOn(command.basedOn)
            (context.copy(freshNameProvider = np), name)
          }
       }
      
    }
  
  val methodBodyCapabilities: MethodBodyCapabilities =
    new MethodBodyCapabilities {
      implicit val canDebugInMethodBody: Understands[MethodBodyContext,Debug] =
        new Understands[MethodBodyContext,Debug] {
          def perform(context: MethodBodyContext, command: Debug): (MethodBodyContext, Unit) = {
            System.err.println (command.tag + ": " + context.method(ast.Name("placeholder_for_a_name")))
          (context,())
          }
        }
      
      implicit val canAddImportInMethodBody: Understands[MethodBodyContext,AddImport[Import]] = 
        new Understands[MethodBodyContext,AddImport[Import]] {
          def perform(context: MethodBodyContext, command: AddImport[Import]): (MethodBodyContext, Unit) = {
            val imports = (context.extraImports :+ command.imp).distinct
            (context.copy(extraImports = imports), ())
          }
        }
      
      implicit val canAddBlockDefinitionsInMethodBody: Understands[MethodBodyContext,AddBlockDefinitions[Statement]] =
        new Understands[MethodBodyContext,AddBlockDefinitions[Statement]] {
          

        }
      
      implicit val canSetReturnTypeInMethodBody: Understands[MethodBodyContext,SetReturnType[Type]] = ???
      
      implicit val canSetParametersInMethodBody: Understands[MethodBodyContext,SetParameters[Name,Type]] = ???
      
      implicit val canTransformTypeInMethodBody: Understands[MethodBodyContext,ToTargetLanguageType[Type]] = ???
      
      implicit def canReifyInMethodBody[T]: Understands[MethodBodyContext,Reify[T,Expression]] = ???
      
      implicit val canResolveImportInMethod: Understands[MethodBodyContext,ResolveImport[Import,Type]] = ???
      
      implicit val canApplyInMethodBody: Understands[MethodBodyContext,Apply[Expression,Expression,Expression]] = ???
      
      implicit val canGetArgumentsInMethodBody: Understands[MethodBodyContext,GetArguments[Type,Name,Expression]] = ???
      
      implicit val canGetFreshNameInMethodBody: Understands[MethodBodyContext,FreshName[Name]] = ???
      
    }


    /*
  val methodBodyCapabilities: MethodBodyCapabilities =
    new MethodBodyCapabilities {
      implicit val canDebugInMethodBody: Understands[MethodBodyCtxt, Debug] =
        new Understands[MethodBodyCtxt, Debug] {
          def perform(
            context: MethodBodyCtxt,
            command: Debug
          ): (MethodBodyCtxt, Unit) = {

            System.err.println (command.tag + ": " + context.method)
            (context,())
          }
        }

      implicit val canAddImportInMethodBody: Understands[MethodBodyCtxt, AddImport[Import]] =
        new Understands[MethodBodyCtxt, AddImport[Import]] {
          def perform(
            context: MethodBodyCtxt,
            command: AddImport[Import]
          ): (MethodBodyCtxt, Unit) = {
            (context.copy(extraImports = (context.extraImports :+ command.imp).groupBy(_.structure).mapValues(_.head).values.toList), ())
          }
        }

      implicit val canAddBlockDefinitionsInMethodBody: Understands[MethodBodyCtxt, AddBlockDefinitions[Statement]] =
        new Understands[MethodBodyCtxt, AddBlockDefinitions[Statement]] {
          def perform(
            context: MethodBodyCtxt,
            command: AddBlockDefinitions[Statement]
          ): (MethodBodyCtxt, Unit) = {
            val newMethod =
              context.method match {
                case definition: Defn.Def =>
                  val newBody =
                    definition.body match {
                      case block: Term.Block =>
                        block.copy(stats =
                          block.stats ++ command.definitions
                        )
                      case exp =>
                        Term.Block((exp +: command.definitions).toList)
                    }
                  definition.copy(body = newBody)
                case decl: Decl.Def =>
                  Defn.Def(
                    mods = decl.mods,
                    name = decl.name,
                    tparams = decl.tparams,
                    paramss = decl.paramss,
                    decltpe = Some(decl.decltpe),
                    body = Term.Block(command.definitions.toList)
                  )
              }
            (context.copy(method = newMethod), ())
          }
        }

      implicit val canSetReturnTypeInMethodBody: Understands[MethodBodyCtxt, SetReturnType[Type]] =
        new Understands[MethodBodyCtxt, SetReturnType[Type]] {
          def perform(
            context: MethodBodyCtxt,
            command: SetReturnType[Type]
          ): (MethodBodyCtxt, Unit) = {
            val updatedMethod =
              context.method match {
                case definition: Defn.Def =>
                  definition.copy(decltpe = Some(command.tpe))
                case decl: Decl.Def =>
                  decl.copy(decltpe = command.tpe)
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
            val params: List[Term.Param] =
              command.params.map {
                case (name, tpe) =>
                  Term.Param(
                    mods = List.empty,
                    name = name.toAST,
                    decltpe = Some(tpe),
                    default = None
                  )
              }.toList

            val updatedMethod =
              context.method match {
                case definition: Defn.Def =>
                  definition.copy(paramss = List(params))
                case decl: Decl.Def =>
                  decl.copy(paramss = List(params))
              }
            (context.copy(method = updatedMethod), ())
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

      implicit val canResolveImportInMethod: Understands[MethodBodyCtxt, ResolveImport[Import, Type]] =
        new Understands[MethodBodyCtxt, ResolveImport[Import, Type]] {
          def perform(
            context: MethodBodyCtxt,
            command: ResolveImport[Import, Type]
          ): (MethodBodyCtxt, Option[Import]) = {
            val stripped = AnyParadigm.stripGenerics(command.forElem)
            Try { (context, context.resolver.importResolution(stripped)) } getOrElse {
              (context, AnyParadigm.guessImport(config.targetPackage.ref, command.forElem))
            }
          }
        }

      implicit val canApplyInMethodBody: Understands[MethodBodyCtxt, Apply[Expression, Expression, Expression]] =
        new Understands[MethodBodyCtxt, Apply[Expression, Expression, Expression]] {
          def perform(
            context: MethodBodyCtxt,
            command: Apply[Expression, Expression, Expression]
          ): (MethodBodyCtxt, Expression) = {
            (context, Term.Apply(command.functional, command.arguments.toList))
          }
        }

      implicit val canGetArgumentsInMethodBody: Understands[MethodBodyCtxt, GetArguments[Type, Name, Expression]] =
        new Understands[MethodBodyCtxt, GetArguments[Type, Name, Expression]] {
          def perform(
            context: MethodBodyCtxt,
            command: GetArguments[Type, Name, Expression]
          ): (MethodBodyCtxt, Seq[(Name, Type, Expression)]) = {
            val params =
              context.method match {
                case definition: Defn.Def => definition.paramss.flatten
                case decl: Decl.Def => decl.paramss.flatten
              }
            val result =
              params.map(param => (MangledName.fromAST(param.name), param.decltpe.get, Term.Name(param.name.value)))
            (context, result)
          }
        }
      implicit val canGetFreshNameInMethodBody: Understands[MethodBodyContext, FreshName[Name]] =
        new Understands[MethodBodyContext, FreshName[Name]] {
          def perform(context: MethodBodyContext, command: FreshName[MangledName]): (MethodBodyContext, MangledName) = {
            (context, MangledName.fromAST(Term.fresh(command.basedOn.toAST.value)))
          }
        }
    }

  val testCapabilities: TestCapabilities =
    new TestCapabilities {
      implicit val canDebugInTest: Understands[TestContext, Debug] =
        new Understands[TestContext, Debug] {
          def perform(
            context: TestContext,
            command: Debug
          ): (TestContext, Unit) = {
            System.err.println (command.tag + ": " + context.testClass)
            (context,())
          }
        }

      implicit val canAddTestCaseInTest: Understands[TestContext, AddTestCase[MethodBodyContext, Name, Expression]] =
        new Understands[TestContext, AddTestCase[MethodBodyContext, Name, Expression]] {
          def perform(
            context: TestContext,
            command: AddTestCase[MethodBodyContext, Name, Expression]
          ): (TestContext, Unit) = {

            val testName = Term.Name(command.name.toAST.value)
            val (resultingContext, assertions) =
              Command.runGenerator(
                command.code,
                MethodBodyCtxt(
                  resolver = context.resolver,
                  extraImports = context.extraImports,
                  Defn.Def(List.empty, testName, List.empty, List.empty, None, Term.Block(List.empty))
                )
              )

            val testBody: Term =
              resultingContext.method match {
                case defn: Defn.Def =>
                  defn.body match {
                    case Term.Block(stats) => Term.Block(stats ++ assertions.toList)
                    case x => Term.Block(x +: assertions.toList)
                  }
                case decl: Decl.Def =>
                  Term.Block(assertions.toList)
              }


            val testDecl =
              Term.Apply(
                Term.Apply(
                  Term.Name("test"),
                  List(Lit.String(testName.value))
                ),
                List(testBody)
              )
            val updatedClass =
              context.testClass.copy(templ =
                context.testClass.templ.copy(stats =
                  context.testClass.templ.stats :+ testDecl
                ))
            (context.copy(resolver = resultingContext.resolver, testClass = updatedClass, extraImports = resultingContext.extraImports), ())
          }
        }
    }

  private val defaultResolver: ContextSpecificResolver = {
    val emptyResolver =
      ContextSpecificResolver(
        _methodTypeResolution = _ => tpe => throw new NotImplementedError(tpe.toString),
        _constructorTypeResolution = _ => tpe => throw new NotImplementedError(tpe.toString),
        _classTypeResolution = _ => tpe => throw new NotImplementedError(tpe.toString),
        _typeTypeResolution = _ => tpe => throw new NotImplementedError(tpe.toString),
        _reificationInConstructor = _ => rep => throw new NotImplementedError(rep.toString),
        _reificationInMethod = _ => rep => throw new NotImplementedError(rep.toString),
        _tpeImportResolution = _ => tpe => throw new NotImplementedError(tpe.toString),
        _termImportResolution = _ => term => throw new NotImplementedError(term.toString),
        _instantiationOverride = _ => (tpe, args) => (tpe, args)
      )
    ContextSpecificResolver.updateResolver(config, TypeRep.Unit, Type.Name("Unit"))(rep => Lit.Unit())(emptyResolver)
  }


  override def runGenerator(generator: Generator[ProjectContext, Unit]): Seq[FileWithPath] = {

    val (finalContext, _) =
      Command.runGenerator(generator,
        ProjectCtxt(
          resolver = defaultResolver,
          units = Seq.empty,
          testUnits = Seq.empty,
          extraDependencies = Seq.empty
        )
      )
    val nameEntry = config.projectName.map(n => s"""name := "${n}"""").getOrElse("")
    val scalaTestDeps = Seq(
      """"org.scalatest" %% "scalatest" % "3.2.9" % "test""""
    )
    val deps = (scalaTestDeps ++ finalContext.extraDependencies).mkString("Seq(\n    ", ",\n    ", "\n  )")
    val buildFile =
      s"""
         |$nameEntry
         |scalaVersion := "3.0.1"
         |libraryDependencies ++= $deps
           """.stripMargin
    // TODO: Add more cleanup (imports?)..
    val cleanedUnits = finalContext.units
    val cleanedTestUnits = finalContext.testUnits
    val scalaFiles = cleanedUnits.map { case (name, unit) =>
      FileWithPath(
        unit.toString,
        AnyParadigm.computePath(Paths.get("src", "main", "scala"), name)
      )
    }
    val scalaTestFiles = cleanedTestUnits.map { case (name, unit) =>
      FileWithPath(
        unit.toString,
        AnyParadigm.computePath(Paths.get("src", "test", "scala"), name)
      )
    }
    val gitIgnore = BundledResource("gitignore", Paths.get(".gitignore"), classOf[CodeGenerator])
    FileWithPath(
      ResourcePersistable.bundledResourceInstance.rawText(gitIgnore),
      ResourcePersistable.bundledResourceInstance.path(gitIgnore)) +:
      FileWithPath(buildFile, Paths.get("build.sbt")) +:
      (scalaFiles ++ scalaTestFiles)
  }*/
}


object AnyParadigm {

}