package org.combinators.ep.language.scala.codegen     /*DI:LD:AI*/

import cats.Apply as _
import org.combinators.cogen
import org.combinators.cogen.Command.Generator
import org.combinators.cogen.{Command, FileWithPath, TypeRep, Understands}
import org.combinators.ep.language.inbetween.ContextRegistry
import org.combinators.ep.language.inbetween.any.*
import org.combinators.ep.language.inbetween.any.AnyParadigm.WithSyntax
import org.combinators.ep.language.inbetween.functional.FunctionalParadigm
import org.combinators.ep.language.inbetween.imperative.Imperative
import org.combinators.ep.language.scala.ast.functional.control
import org.combinators.ep.language.inbetween.oo.OOParadigm
import org.combinators.ep.language.inbetween.polymorphism.generics.Generics
import org.combinators.ep.language.inbetween.polymorphism.{ParametricPolymorphism, ParametricPolymorphismInADTContexts}
import org.combinators.ep.language.scala.ast.ffi.{Arithmetic, ArithmeticAST, Arrays, ArraysAST, Assertions, AssertionsAST, BooleanAST, Booleans, Console, ConsoleAST, Equals, EqualsAST, Exceptions, ExceptionsAST, Lists, ListsAST, Maps, MapsAST, OperatorExpressionsAST, RealArithmetic, RealArithmeticAST, StringAST, Strings, UnitAST, Units}
import org.combinators.ep.language.scala.ast.{BaseAST, NameProviderAST}

import java.nio.file.{Path, Paths}

type FullAST = BaseAST
  & NameProviderAST
  & ArithmeticAST
  & ArraysAST
  & AssertionsAST
  & BooleanAST
  & ConsoleAST
  & EqualsAST
  & ExceptionsAST
  & ListsAST
  & MapsAST
  & OperatorExpressionsAST
  & RealArithmeticAST
  & StringAST
  & UnitAST
/**
 * Scala-specific.
 *
 * These paradigm-specific traits are conceptually different from each other
 */
sealed class CodeGenerator[AST <: FullAST](val domainName: String, val ast: AST, additionalPrefixExcludedTypes: Set[Seq[ast.any.Name]] = Set.empty) { cc =>
  val syntax: AbstractSyntax.WithAST[ast.type] = AbstractSyntax(ast)
  val nameProvider: ast.nameProvider.ScalaNameProvider = ast.nameProviderFactory.scalaNameProvider

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
  }

  def runGenerator(generator: Generator[ast.any.Project, Unit]): Seq[FileWithPath] = {
    val emptyProject: ast.any.Project = ast.scalaBaseFactory.scalaProject(Set.empty)

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
    val (generatedProject, _) = Command.runGenerator(generator, emptyProject)
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

  val methodRegistry: ContextRegistry[paradigm.type, paradigm.MethodBodyContext] = {
    class Reg(override val base: paradigm.type = paradigm) extends ContextRegistry[paradigm.type, paradigm.MethodBodyContext] {
      override def enable(
        ffi: org.combinators.cogen.paradigm.ffi.FFI,
        tpeLookup: TypeRep => Option[Generator[paradigm.MethodBodyContext, paradigm.syntax.Type]],
        reifylookup: (tpe:TypeRep) => tpe.HostType => Option[Generator[paradigm.MethodBodyContext, this.base.syntax.Expression]],
      ): Generator[paradigm.ProjectContext, Boolean] = {
        object Enable extends cogen.Command {
          type Result = Boolean
        }
        val canEnable = new Understands[paradigm.ProjectContext, Enable.type] {
          override def perform(context: paradigm.ast.any.Project, command: Enable.type): (paradigm.ast.any.Project, Boolean) = {
            if (!context.enabledFFIs.contains(ffi)) {
              (context.addTypeLookupsForMethods(tpeLookup).addReifyLookupsForMethods(reifylookup).markEnabledFFI(ffi), true)
            } else {
              (context, false)
            }
          }
        }
        cogen.paradigm.AnyParadigm.capability[paradigm.ProjectContext, Boolean, Enable.type](Enable)(using canEnable)
      }
    }
    new Reg()
  }

  val constructorRegistry: ContextRegistry[paradigm.type, paradigm.ast.oo.Constructor] = {
    class Reg(override val base: paradigm.type = paradigm) extends ContextRegistry[paradigm.type, paradigm.ast.oo.Constructor] {
      override def enable(
        ffi: org.combinators.cogen.paradigm.ffi.FFI,
        tpeLookup: TypeRep => Option[Generator[paradigm.ast.oo.Constructor, paradigm.syntax.Type]],
        reifylookup: (tpe: TypeRep) => tpe.HostType => Option[Generator[paradigm.ast.oo.Constructor, this.base.syntax.Expression]],
      ): Generator[paradigm.ProjectContext, Boolean] = {
        import paradigm.ast.factory._
        object Enable extends cogen.Command {
          type Result = Boolean
        }
        val canEnable = new Understands[paradigm.ProjectContext, Enable.type] {
          override def perform(context: paradigm.ast.any.Project, command: Enable.type): (paradigm.ast.any.Project, Boolean) = {
            if (!context.enabledFFIs.contains(ffi)) {
              (context.addTypeLookupsForConstructors(tpeLookup).addReifyLookupsForConstructors(reifylookup), true)
            } else {
              (context, false)
            }
          }
        }
        cogen.paradigm.AnyParadigm.capability[paradigm.ProjectContext, Boolean, Enable.type](Enable)(using canEnable)
      }
    }
    new Reg()
  }
  val classRegistry: ContextRegistry[paradigm.type, paradigm.ast.oo.Class] = {
    class Reg(override val base: paradigm.type = paradigm) extends ContextRegistry[paradigm.type, paradigm.ast.oo.Class] {
      override def enable(
        ffi: org.combinators.cogen.paradigm.ffi.FFI,
        tpeLookup: TypeRep => Option[Generator[paradigm.ast.oo.Class, paradigm.syntax.Type]],
        reifylookup: (tpe: TypeRep) => tpe.HostType => Option[Generator[paradigm.ast.oo.Class, this.base.syntax.Expression]],
      ): Generator[paradigm.ProjectContext, Boolean] = {
        import paradigm.ast.factory._
        object Enable extends cogen.Command {
          type Result = Boolean
        }
        val canEnable = new Understands[paradigm.ProjectContext, Enable.type] {
          override def perform(context: paradigm.ast.any.Project, command: Enable.type): (paradigm.ast.any.Project, Boolean) = {
            if (!context.enabledFFIs.contains(ffi)) {
              (context.addTypeLookupsForClasses(tpeLookup).addReifyLookupsForClasses(reifylookup), true)
            } else {
              (context, false)
            }
          }
        }
        cogen.paradigm.AnyParadigm.capability[paradigm.ProjectContext, Boolean, Enable.type](Enable)(using canEnable)
      }
    }
    new Reg()
  }
  val ooParadigm: OOParadigm.WithBase[paradigm.type] = OOParadigm(paradigm)
  val imperative: Imperative.WithBase[paradigm.type] = Imperative[ast.type, paradigm.type](paradigm)

  val functional: FunctionalParadigm.WithBase[paradigm.type] = FunctionalParadigm[ast.type, paradigm.type](paradigm)

  val parametricPolymorphism: ParametricPolymorphism.WithBase[paradigm.type] = ParametricPolymorphism[ast.type, paradigm.type](paradigm)
  val generics: Generics.WithBase[paradigm.type, ooParadigm.type, parametricPolymorphism.type] = Generics[ast.type, paradigm.type](paradigm, ooParadigm, parametricPolymorphism)
  val parametricPolymorphismInADTContexts: ParametricPolymorphismInADTContexts.WithBase[paradigm.type, functional.type] = ParametricPolymorphismInADTContexts[ast.type, paradigm.type](paradigm, functional)

  val functionalControl: control.Functional.WithBase[paradigm.type, parametricPolymorphism.type, ooParadigm.type, generics.type] =
    control.Functional[ast.type, paradigm.type](
      paradigm,
      parametricPolymorphism,
      ooParadigm,
      generics,
      methodRegistry,
      constructorRegistry,
      classRegistry
    )

  val arrays: Arrays.WithBase[paradigm.type, parametricPolymorphism.type, ooParadigm.type, generics.type] = Arrays[ast.type, paradigm.type](
    paradigm,
    parametricPolymorphism,
    ooParadigm,
    generics,
    methodRegistry,
    constructorRegistry,
    classRegistry,
  )
  
  val booleans: Booleans.WithBase[paradigm.type] = Booleans[ast.type, paradigm.type](
    paradigm,
    methodRegistry,
    constructorRegistry,
    classRegistry,
  )
  
  val units: Units.WithBase[paradigm.type] = Units[ast.type, paradigm.type](
    paradigm,
    methodRegistry,
    constructorRegistry,
    classRegistry,
  )
  
  val doubles: Arithmetic.WithBase[Double, paradigm.type] = Arithmetic[ast.type, paradigm.type, Double](
    paradigm,
    TypeRep.Double,
    methodRegistry,
    constructorRegistry,
    classRegistry,
    "Double"
  )
  
  val console: Console.WithBase[paradigm.type] = Console[ast.type, paradigm.type](
    paradigm,
    methodRegistry,
    constructorRegistry,
    classRegistry,
  )
  
  val realDoubles: RealArithmetic.WithBase[paradigm.type, Double] = RealArithmetic[ast.type, paradigm.type, Double](
    paradigm,
    TypeRep.Double,
    methodRegistry,
    constructorRegistry,
    classRegistry,
    "Double"
  )

  val ints: Arithmetic.WithBase[Int, paradigm.type] = Arithmetic[ast.type, paradigm.type, Int](
    paradigm,
    TypeRep.Int,
    methodRegistry,
    constructorRegistry,
    classRegistry,
    "Int"
  )

  val equality: Equals.WithBase[paradigm.type] = Equals[ast.type, paradigm.type](
      paradigm,
      methodRegistry,
      constructorRegistry,
      classRegistry,
  )
  
  val strings: Strings.WithBase[paradigm.type] = Strings[ast.type, paradigm.type](
    paradigm,
    methodRegistry,
    constructorRegistry,
    classRegistry,
  )

  val lists: Lists.WithBase[paradigm.type, parametricPolymorphism.type, ooParadigm.type, generics.type] = Lists[ast.type, paradigm.type](
    paradigm,
    parametricPolymorphism,
    ooParadigm,
    generics,
    methodRegistry,
    constructorRegistry,
    classRegistry,
  )
  val maps: Maps.WithBase[paradigm.type, parametricPolymorphism.type, ooParadigm.type, generics.type] = Maps[ast.type, paradigm.type](
    paradigm,
    parametricPolymorphism,
    ooParadigm,
    generics,
    methodRegistry,
    constructorRegistry,
    classRegistry,
  )

  val assertions: Assertions.WithBase[paradigm.type] = Assertions[ast.type, paradigm.type](
    paradigm,
    methodRegistry,
    constructorRegistry,
    classRegistry,
  )
  
  val exceptions: Exceptions.WithBase[paradigm.type] = Exceptions[ast.type, paradigm.type](
    paradigm,
    methodRegistry,
    constructorRegistry,
    classRegistry,
  )
  
  def enableDefaultFFIs():Generator[paradigm.ProjectContext, Unit] = {
    for {
      _ <- functionalControl.arrowsInMethods.enable()
      _ <- functionalControl.arrowsInClasses.enable()
      _ <- functionalControl.arrowsInConstructors.enable()

      _ <- units.unitsInClasses.enable()
      _ <- units.unitsInMethods.enable()
      _ <- units.unitsInConstructors.enable()

      _ <- doubles.arithmeticInMethods.enable()
      _ <- doubles.arithmeticInClasses.enable()
      _ <- doubles.arithmeticInConstructors.enable()

      _ <- realDoubles.realArithmeticInMethods.enable()
      _ <- realDoubles.realArithmeticInClasses.enable()
      _ <- realDoubles.realArithmeticInConstructors.enable()

      _ <- ints.arithmeticInMethods.enable()
      _ <- ints.arithmeticInClasses.enable()
      _ <- ints.arithmeticInConstructors.enable()

      _ <- strings.stringsInMethods.enable()
      _ <- strings.stringsInClasses.enable()
      _ <- strings.stringsInConstructors.enable()

      _ <- lists.listsInMethods.enable()
      _ <- lists.listsInClasses.enable()
      _ <- lists.listsInConstructors.enable()

      _ <- console.consoleInMethods.enable()
      _ <- console.consoleInClasses.enable()
      _ <- console.consoleInConstructors.enable()

      _ <- arrays.arraysInMethods.enable()
      _ <- arrays.arraysInClasses.enable()
      _ <- arrays.arraysInConstructors.enable()

      _ <- equality.equalsInMethods.enable()
      _ <- equality.equalsInClasses.enable()
      _ <- equality.equalsInConstructors.enable()

      _ <- assertions.assertionsInMethods.enable()
      _ <- assertions.assertionsInClasses.enable()
      _ <- assertions.assertionsInConstructors.enable()

      _ <- booleans.booleansInMethods.enable()
      _ <- booleans.booleansInClasses.enable()
      _ <- booleans.booleansInConstructors.enable()

      _ <- exceptions.exceptionsInMethods.enable()
      _ <- exceptions.exceptionsInClasses.enable()
      _ <- exceptions.exceptionsInConstructors.enable()
      
      _ <- maps.mapsInMethods.enable()
      _ <- maps.mapsInClasses.enable()
      _ <- maps.mapsInConstructors.enable()

    } yield ()
  }
}

object CodeGenerator {

  case object Enable extends Command {
    type Result = Unit
  }

  def apply[AST <: FullAST](domainName: String, ast: AST, additionalPrefixExcludedTypes: Set[Seq[ast.any.Name]] = Set.empty): CodeGenerator[ast.type] =
    new CodeGenerator[ast.type](domainName, ast, additionalPrefixExcludedTypes)
}
