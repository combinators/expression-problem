package org.combinators.ep.language.scala.codegen     /*DI:LD:AI*/

import cats.Apply as _
import org.combinators.cogen
import org.combinators.cogen.Command.Generator
import org.combinators.cogen.paradigm.{Apply, ToTargetLanguageType, ffi}
import org.combinators.cogen.{Command, FileWithPath, TypeRep, Understands}
import org.combinators.ep.language.inbetween.ContextRegistry
import org.combinators.ep.language.inbetween.any.*
import org.combinators.ep.language.inbetween.any.AnyParadigm.WithSyntax
import org.combinators.ep.language.inbetween.ffi.*
import org.combinators.ep.language.inbetween.functional.control.Functional.WithBase
import org.combinators.ep.language.inbetween.functional.{FunctionalParadigm, control}
import org.combinators.ep.language.inbetween.imperative.Imperative
import org.combinators.ep.language.inbetween.oo.OOParadigm
import org.combinators.ep.language.inbetween.polymorphism.generics.Generics
import org.combinators.ep.language.inbetween.polymorphism.{ParametricPolymorphism, ParametricPolymorphismInADTContexts}
import org.combinators.ep.language.scala.ast.ffi.{Arithmetic, ArithmeticAST}
import org.combinators.ep.language.scala.ast.{BaseAST, NameProviderAST}

import java.nio.file.{Path, Paths}

type FullAST = BaseAST
  & NameProviderAST
  & ArraysAST
  & ArithmeticAST
  & AssertionsAST
  & BooleanAST
  & ConsoleAST
  & ExceptionsAST
  & EqualsAST
  & ListsAST
  & MapsAST
  & OperatorExpressionOpsAST
  & RealArithmeticAST
  & StringAST
/**
 * Scala-specific.
 *
 * These paradigm-specific traits are conceptually different from each other
 */
sealed class CodeGenerator[AST <: FullAST](val domainName: String, val ast: AST, additionalPrefixExcludedTypes: Set[Seq[ast.any.Name]] = Set.empty) { cc =>
  val syntax: AbstractSyntax.AbstractSyntax[ast.type] = AbstractSyntax(ast)
  val nameProvider: ast.nameProvider.ScalaNameProvider = ast.nameProviderFactory.scalaNameProvider

  

  /*def toLookup[Ctxt](name: String*): Option[Generator[Ctxt, ast.any.Type]] = {
    Some(Command.lift(ast.ooFactory.classReferenceType(name.map(nameProvider.mangle)*)))
  }

  def addLookupsForImplementedGenerators[Ctxt](
    project: ast.any.Project,
    add: (ast.any.Project, TypeRep => Option[Generator[Ctxt, ast.any.Type]]) => ast.any.Project
  )(implicit canToTargetLanguage: Understands[Ctxt, ToTargetLanguageType[ast.any.Type]],
    canApplyType: Understands[Ctxt, Apply[ast.any.Type, ast.any.Type, ast.any.Type]]
  ): ast.any.Project = {
    add(project, {
      case TypeRep.Double => toLookup("Double")
      case TypeRep.Int => toLookup("Int")
      case TypeRep.Boolean => toLookup("Boolean")
      case TypeRep.String => toLookup("String")
      case TypeRep.Unit => toLookup("Unit")
      case TypeRep.Array(elemTpe) =>
        Some(
          for {
            elemTpe <- ToTargetLanguageType[ast.any.Type](elemTpe).interpret(using canToTargetLanguage)
            arrayTpe <- Command.lift(ast.arraysOpsFactory.array())
            tpe <- Apply[
              ast.any.Type,
              ast.any.Type,
              ast.any.Type](arrayTpe, Seq(elemTpe)).interpret(using canApplyType)
          } yield tpe)
      case TypeRep.Map(keyTpe, elemTpe) =>
        Some(
          for {
            keyTpe <- ToTargetLanguageType[ast.any.Type](keyTpe).interpret(using canToTargetLanguage)
            elemTpe <- ToTargetLanguageType[ast.any.Type](elemTpe).interpret(using canToTargetLanguage)
            mapTpe <- Command.lift(ast.mapsOpsFactory.map())
            tpe <- Apply[
              ast.any.Type,
              ast.any.Type,
              ast.any.Type](mapTpe, Seq(keyTpe, elemTpe)).interpret(using canApplyType)
          } yield tpe)
      case TypeRep.Sequence(elemTpeRep) =>
        Some(
          for {
            elemTpe <- ToTargetLanguageType[ast.any.Type](elemTpeRep).interpret(using canToTargetLanguage)
            seqTpe <- Command.lift(ast.ooFactory.classReferenceType(nameProvider.mangle("Seq")))
            tpe <- Apply[
              ast.any.Type,
              ast.any.Type,
              ast.any.Type](seqTpe, Seq(elemTpe)).interpret(using canApplyType)
          } yield tpe)
      case TypeRep.Arrow(src, tgt) =>
        Some(
          for {
            srcTpe <- ToTargetLanguageType[ast.any.Type](src).interpret(using canToTargetLanguage)
            tgtTpe <- ToTargetLanguageType[ast.any.Type](tgt).interpret(using canToTargetLanguage)
            funTpe <- Command.lift(ast.ooFactory.classReferenceType(nameProvider.mangle("Function")))
            tpe <- Apply[
              ast.any.Type,
              ast.any.Type,
              ast.any.Type](funTpe, Seq(srcTpe, tgtTpe)).interpret(using canApplyType)
          } yield tpe)
      case _ => None
    })
  }

  def prefixExcludedTypes: Set[Seq[ast.any.Name]] = {
    Set(
      Seq("Array"),
      Seq("Double"),
      Seq("Boolean"),
      Seq("Int"),
      Seq("Map"),
      Seq("Unit"),
      Seq("String"),
      Seq("Seq"),
      Seq("Function"),
    ).map(qname => qname.map(nameProvider.mangle)) ++ additionalPrefixExcludedTypes
  } */

  def runGenerator(generator: Generator[ast.any.Project, Unit]): Seq[FileWithPath] = {
    var projectWithLookups: ast.any.Project = ast.scalaBaseFactory.scalaProject(Set.empty)

    def buildFile: FileWithPath = {
      // create a rudimentary build.sbt for Scala just to work with sbt version 1.7.1
      // https://www.baeldung.com/scala/sbt-scoverage-code-analysis
      val cmds = s"""
                    |val sopts = Seq(
                    |  "-coverage-out:coverage"
                    |)
                    |val soptsNoTest = Seq(
                    |)
                    |
                    |Compile / scalacOptions ++= sopts ++ soptsNoTest
                    |Test / scalacOptions ++= sopts
                    |
                    |scalaVersion := "3.3.3"
                    |coverageEnabled := true
                    |libraryDependencies ++= Seq(
                    |    "org.scalactic" %% "scalactic" % "3.2.19" % "test",
                    |    "org.scalatest" %% "scalatest" % "3.2.19" % "test",
                    |  )
           """.stripMargin
      FileWithPath(cmds, Paths.get("build.sbt"))
    }

    def pluginsFile: FileWithPath = {
      val plugins =
        s"""
           |addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.2")
           |addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.0.8")
           |
           |ThisBuild / libraryDependencySchemes += "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always
           |""".stripMargin
      FileWithPath(plugins, Paths.get("project", "plugins.sbt"))
    }

    def scalaFmt: FileWithPath = {
      val fmt =
        s"""
           |version = 3.8.3
           |runner.dialect = scala3
           |""".stripMargin
      FileWithPath(fmt, Paths.get(".scalafmt.conf"))
    }


    projectWithLookups =
      addLookupsForImplementedGenerators[ast.any.Method](
        ast.factory.convert(projectWithLookups),
        { case (project, lookup) => ast.factory.convert(project).addTypeLookupsForMethods(lookup) }
      )(using paradigm.methodBodyCapabilities.canTransformTypeInMethodBody,
        parametricPolymorphism.methodBodyCapabilities.canApplyTypeInMethod)
    projectWithLookups =
      addLookupsForImplementedGenerators[ast.any.Method](
        ast.factory.convert(projectWithLookups),
        { case (project, lookup) => ast.factory.convert(project).addTypeLookupsForFunctions(lookup) }
      )(using paradigm.methodBodyCapabilities.canTransformTypeInMethodBody,
        parametricPolymorphism.methodBodyCapabilities.canApplyTypeInMethod)
    projectWithLookups =
      addLookupsForImplementedGenerators[ast.oo.Class](
        ast.factory.convert(projectWithLookups),
        { case (project, lookup) => ast.factory.convert(project).addTypeLookupsForClasses(lookup) }
      )(using ooParadigm.classCapabilities.canTranslateTypeInClass,
        generics.classCapabilities.canApplyTypeInClass)
    projectWithLookups =
      addLookupsForImplementedGenerators[ast.oo.Constructor](
        ast.factory.convert(projectWithLookups),
        { case (project, lookup) => ast.factory.convert(project).addTypeLookupsForConstructors(lookup) }
      )(using ooParadigm.constructorCapabilities.canTranslateTypeInConstructor,
        generics.constructorCapabilities.canApplyTypeInConstructor)
    projectWithLookups =
      addLookupsForImplementedGenerators[ast.functional.AlgebraicDataType](
        ast.factory.convert(projectWithLookups),
        { case (project, lookup) => ast.factory.convert(project).addTypeLookupsForAlgebraicDataTypes(lookup) }
      )(using functional.typeCapabilities.canTranslateTypeInType,
        parametricPolymorphismInADTContexts.algebraicDataTypeCapabilities.canApplyTypeInADT)

    val (generatedProject, _) = Command.runGenerator(generator, projectWithLookups)
    val withPrefix = ast.factory.convert(generatedProject).prefixRootPackage(Seq(nameProvider.mangle(domainName)), prefixExcludedTypes)

    def toFileWithPath(cu: ast.any.CompilationUnit, basePath: Path): FileWithPath = {
      FileWithPath(ast.factory.convert(cu).toScala, {
        val nameAsStrings = cu.name.map(name => ast.factory.convert(name).toScala)
        val nameWithScalaExtension = nameAsStrings.init :+ (nameAsStrings.last + ".scala")
        nameWithScalaExtension.foldLeft(basePath)({ case (path, name) =>
          Paths.get(path.toString, name)
        })
      })
    }
    val mainDir = Paths.get("src", "main", "scala")
    val testDir = Paths.get("src", "test", "scala")
    withPrefix.compilationUnits.flatMap(cu => {
      import ast.factory.*
      val testFile = if (cu.tests.nonEmpty) {
        val testOnlyCu = cu.copyAsCompilationUnitWithClasses(
          classes = Seq.empty
        )
        Seq(toFileWithPath(testOnlyCu, testDir))
      } else Seq.empty
      val nonTestFile = if (cu.classes.nonEmpty || cu.functions.nonEmpty || cu.adts.nonEmpty) {
        val noTestCu = cu.copy(
          tests = Seq.empty
        )
        Seq(toFileWithPath(noTestCu, mainDir))
      } else Seq.empty

      nonTestFile ++ testFile
    }).toSeq ++ withPrefix.customFiles :+ buildFile :+ pluginsFile :+ scalaFmt
  }

  val paradigm: WithSyntax[ast.type, syntax.type] = AnyParadigm[ast.type, syntax.type](ast, runGenerator, syntax)

  val methodRegistry: ContextRegistry[paradigm.type, paradigm.MethodBodyContext] = new ContextRegistry[paradigm.type, paradigm.MethodBodyContext](paradigm) {
    override def enable(
      ffi: org.combinators.cogen.paradigm.ffi.FFI,
      tpeLookup: TypeRep => Option[Generator[paradigm.MethodBodyContext, paradigm.syntax.Type]],
      reifylookup: (tpe:TypeRep) => tpe.HostType => Option[Generator[paradigm.MethodBodyContext, this.base.syntax.Expression]],
    ): Generator[paradigm.ProjectContext, Unit] = {
      object Enable extends cogen.Command {
        type Result = Unit
      }
      val canEnable = new Understands[paradigm.ProjectContext, Enable.type] {
        override def perform(context: paradigm.ast.any.Project, command: Enable.type): (paradigm.ast.any.Project, Unit) = {
          (context.addTypeLookupsForMethods(tpeLookup).addReifyLookupsForMethods(reifylookup), ())
        }
      }
      cogen.paradigm.AnyParadigm.capability[paradigm.ProjectContext, Unit, Enable.type](Enable)(using canEnable)
    }
  }
  val constructorRegistry: ContextRegistry[paradigm.type, paradigm.ast.oo.Constructor] = new ContextRegistry[paradigm.type, paradigm.ast.oo.Constructor](paradigm) {
    override def enable(
      ffi: org.combinators.cogen.paradigm.ffi.FFI,
      tpeLookup: TypeRep => Option[Generator[paradigm.ast.oo.Constructor, paradigm.syntax.Type]],
      reifylookup: (tpe:TypeRep) => tpe.HostType => Option[Generator[paradigm.ast.oo.Constructor, this.base.syntax.Expression]],
    ): Generator[paradigm.ProjectContext, Unit] = {
      import paradigm.ast.factory._
      object Enable extends cogen.Command {
        type Result = Unit
      }
      val canEnable = new Understands[paradigm.ProjectContext, Enable.type] {
        override def perform(context: paradigm.ast.any.Project, command: Enable.type): (paradigm.ast.any.Project, Unit) = {
          (context.addTypeLookupsForConstructors(tpeLookup).addReifyLookupsForConstructors(reifylookup), ())
        }
      }
      cogen.paradigm.AnyParadigm.capability[paradigm.ProjectContext, Unit, Enable.type](Enable)(using canEnable)
    }
  }
  val classRegistry: ContextRegistry[paradigm.type, paradigm.ast.oo.Class] = new ContextRegistry[paradigm.type, paradigm.ast.oo.Class](paradigm) {
    override def enable(
      ffi: org.combinators.cogen.paradigm.ffi.FFI,
      tpeLookup: TypeRep => Option[Generator[paradigm.ast.oo.Class, paradigm.syntax.Type]],
      reifylookup: (tpe:TypeRep) => tpe.HostType => Option[Generator[paradigm.ast.oo.Class, this.base.syntax.Expression]],
    ): Generator[paradigm.ProjectContext, Unit] = {
      import paradigm.ast.factory._
      object Enable extends cogen.Command {
        type Result = Unit
      }
      val canEnable = new Understands[paradigm.ProjectContext, Enable.type] {
        override def perform(context: paradigm.ast.any.Project, command: Enable.type): (paradigm.ast.any.Project, Unit) = {
          (context.addTypeLookupsForClasses(tpeLookup).addReifyLookupsForClasses(reifylookup), ())
        }
      }
      cogen.paradigm.AnyParadigm.capability[paradigm.ProjectContext, Unit, Enable.type](Enable)(using canEnable)
    }
  }  
  val ooParadigm: OOParadigm.WithBase[ast.type, paradigm.type] = OOParadigm[ast.type, paradigm.type](paradigm)
  val imperative: Imperative.WithBase[ast.type, paradigm.type] = Imperative[ast.type, paradigm.type](paradigm)
  val functional: FunctionalParadigm.WithBase[ast.type, paradigm.type] = FunctionalParadigm[ast.type, paradigm.type](paradigm)
  val functionalControl: WithBase[ast.type, paradigm.type] = control.Functional[ast.type, paradigm.type](paradigm)

  val parametricPolymorphism: ParametricPolymorphism.WithBase[ast.type, paradigm.type] = ParametricPolymorphism[ast.type, paradigm.type](paradigm)
  val generics: Generics.WithBase[ast.type, paradigm.type, ooParadigm.type, parametricPolymorphism.type] = Generics[ast.type, paradigm.type, ooParadigm.type, parametricPolymorphism.type](paradigm, ooParadigm, parametricPolymorphism)
  val parametricPolymorphismInADTContexts: ParametricPolymorphismInADTContexts.WithBase[ast.type, paradigm.type, functional.type] = ParametricPolymorphismInADTContexts[ast.type, paradigm.type, functional.type](paradigm, functional)

  val arrays: Arrays.WithBase[ast.type, paradigm.type] = Arrays[ast.type, paradigm.type](paradigm)
  val booleans: Booleans.WithBase[ast.type, paradigm.type] = Booleans[ast.type, paradigm.type](paradigm)

  val doubles: Arithmetic.WithBase[Double, ast.type, paradigm.type] = Arithmetic[Double, ast.type, paradigm.type](
    paradigm,
    TypeRep.Double,
    methodRegistry,
    constructorRegistry,
    classRegistry,
  )
  val console: Console.WithBase[ast.type, paradigm.type] = Console[ast.type, paradigm.type](paradigm)
  val realDoubles: RealArithmetic.WithBase[Double, ast.type, paradigm.type] = RealArithmetic[Double, ast.type, paradigm.type](paradigm)

  val ints: Arithmetic.WithBase[Int, ast.type, paradigm.type] = Arithmetic[Int, ast.type, paradigm.type](paradigm)

  val strings: Strings.WithBase[ast.type, paradigm.type] = Strings[ast.type, paradigm.type](paradigm)

  val equality: Equals.WithBase[ast.type, paradigm.type] = Equals[ast.type, paradigm.type](paradigm)

  val lists: Lists.WithBase[ast.type, paradigm.type] = Lists[ast.type, paradigm.type](paradigm)
  val maps: Maps.WithBase[ast.type, paradigm.type] = Maps[ast.type, paradigm.type](paradigm)

  val assertions = Assertions[ast.type, paradigm.type](paradigm)
  val exceptions: Exceptions.WithBase[ast.type, paradigm.type] = Exceptions[ast.type, paradigm.type](paradigm)
}

object CodeGenerator {

  case object Enable extends Command {
    type Result = Unit
  }

  def apply[AST <: FullAST](domainName: String, ast: AST, additionalPrefixExcludedTypes: Set[Seq[ast.any.Name]] = Set.empty): CodeGenerator[ast.type] =
    new CodeGenerator[ast.type](domainName, ast, additionalPrefixExcludedTypes)
}
