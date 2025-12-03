package org.combinators.ep.language.scala.codegen

/*DI:LD:AI*/

import cats.Apply as _
import org.combinators.cogen.Command.Generator
import org.combinators.cogen.paradigm.{Apply, ToTargetLanguageType}
import org.combinators.cogen.{Command, FileWithPath, TypeRep, Understands}
import org.combinators.ep.domain.abstractions.DomainTpeRep
import org.combinators.ep.language.inbetween.any.*
import org.combinators.ep.language.inbetween.ffi.*
import org.combinators.ep.language.inbetween.functional.{FunctionalParadigm2, control}
import org.combinators.ep.language.inbetween.imperative.Imperative2
import org.combinators.ep.language.inbetween.oo.OOParadigm2
import org.combinators.ep.language.inbetween.polymorphism.generics.Generics2
import org.combinators.ep.language.inbetween.polymorphism.{ParametricPolymorphism2, ParametricPolymorphismInADTContexts2}
import org.combinators.ep.language.scala.ast.ffi.ArithmeticAST
import org.combinators.ep.language.scala.ast.{BaseAST, NameProviderAST}

import java.nio.file.{Path, Paths}

type FullAST = BaseAST
  & NameProviderAST
  & ArithmeticAST
  & AssertionsAST
  & BooleanAST
  & EqualsAST
  & ListsAST
  & OperatorExpressionOpsAST
  & RealArithmeticAST
  & StringsAST
/**
 * Scala-specific.
 *
 * These paradigm-specific traits are conceptually different from each other
 */
sealed class CodeGenerator2[AST <: FullAST](val domainName: String, val ast: AST) { cc =>
  val syntax: AbstractSyntax2.AbstractSyntax[ast.type] = AbstractSyntax2(ast)
  val nameProvider: ast.nameProvider.ScalaNameProvider = ast.nameProviderFactory.scalaNameProvider

  def toLookup[Ctxt](name: String*): Option[Generator[Ctxt, ast.any.Type]] = {
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
      case TypeRep.Sequence(elemTpeRep) =>
        Some(
          for {
            elemTpe <- ToTargetLanguageType[ast.any.Type](elemTpeRep).interpret(canToTargetLanguage)
            seqTpe <- Command.lift(ast.ooFactory.classReferenceType(nameProvider.mangle("Seq")))
            tpe <- Apply[
              ast.any.Type,
              ast.any.Type,
              ast.any.Type](seqTpe, Seq(elemTpe)).interpret(canApplyType)
          } yield tpe)
      case TypeRep.Arrow(src, tgt) =>
        Some(
          for {
            srcTpe <- ToTargetLanguageType[ast.any.Type](src).interpret(canToTargetLanguage)
            tgtTpe <- ToTargetLanguageType[ast.any.Type](tgt).interpret(canToTargetLanguage)
            funTpe <- Command.lift(ast.ooFactory.classReferenceType(nameProvider.mangle("Function")))
            tpe <- Apply[
              ast.any.Type,
              ast.any.Type,
              ast.any.Type](funTpe, Seq(srcTpe, tgtTpe)).interpret(canApplyType)
          } yield tpe)
      case _ => None
    })
  }

  def prefixExcludedTypes: Set[Seq[ast.any.Name]] = {
    Set(
      Seq("Double"),
      Seq("Boolean"),
      Seq("Int"),
      Seq("Unit"),
      Seq("String"),
      Seq("Seq"),
      Seq("Function"),
      Seq("org", "combinators", "ep", "util", "Tree"),
      Seq("org", "combinators", "ep", "util", "Node"),
      Seq("org", "combinators", "ep", "util", "Leaf")
    ).map(qname => qname.map(nameProvider.mangle))
  }

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
      )(paradigm.methodBodyCapabilities.canTransformTypeInMethodBody,
        parametricPolymorphism.methodBodyCapabilities.canApplyTypeInMethod)
    projectWithLookups =
      addLookupsForImplementedGenerators[ast.any.Method](
        ast.factory.convert(projectWithLookups),
        { case (project, lookup) => ast.factory.convert(project).addTypeLookupsForFunctions(lookup) }
      )(paradigm.methodBodyCapabilities.canTransformTypeInMethodBody,
        parametricPolymorphism.methodBodyCapabilities.canApplyTypeInMethod)
    projectWithLookups =
      addLookupsForImplementedGenerators[ast.oo.Class](
        ast.factory.convert(projectWithLookups),
        { case (project, lookup) => ast.factory.convert(project).addTypeLookupsForClasses(lookup) }
      )(ooParadigm.classCapabilities.canTranslateTypeInClass,
        generics.classCapabilities.canApplyTypeInClass)
    projectWithLookups =
      addLookupsForImplementedGenerators[ast.oo.Constructor](
        ast.factory.convert(projectWithLookups),
        { case (project, lookup) => ast.factory.convert(project).addTypeLookupsForConstructors(lookup) }
      )(ooParadigm.constructorCapabilities.canTranslateTypeInConstructor,
        generics.constructorCapabilities.canApplyTypeInConstructor)
    projectWithLookups =
      addLookupsForImplementedGenerators[ast.functional.AlgebraicDataType](
        ast.factory.convert(projectWithLookups),
        { case (project, lookup) => ast.factory.convert(project).addTypeLookupsForAlgebraicDataTypes(lookup) }
      )(functional.typeCapabilities.canTranslateTypeInType,
        parametricPolymorphismInADTContexts.algebraicDataTypeCapabilities.canApplyTypeInADT)


    val (generatedProject, _) = Command.runGenerator(generator, projectWithLookups)
    val withPrefix =ast.factory.convert(generatedProject).prefixRootPackage(Seq(nameProvider.mangle(domainName)), prefixExcludedTypes)


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
    }).toSeq :+ buildFile :+ pluginsFile :+ scalaFmt
  }

  val paradigm = AnyParadigm2[ast.type, AbstractSyntax2.AbstractSyntax](ast, runGenerator, syntax)
  val ooParadigm = OOParadigm2[ast.type, paradigm.type](paradigm)
  val imperative = Imperative2[ast.type, paradigm.type](paradigm)
  val functional = FunctionalParadigm2[ast.type, paradigm.type](paradigm)
  val functionalControl = control.Functional2[ast.type, paradigm.type](paradigm)

  val parametricPolymorphism = ParametricPolymorphism2[ast.type, paradigm.type](paradigm)
  val generics = Generics2[ast.type, paradigm.type, OOParadigm2.WithBase, ParametricPolymorphism2.WithBase](paradigm, ooParadigm, parametricPolymorphism)
  val parametricPolymorphismInADTContexts = ParametricPolymorphismInADTContexts2[ast.type, paradigm.type, FunctionalParadigm2.WithBase](paradigm, functional)


  val booleans = Booleans2[ast.type, paradigm.type](paradigm)

  val doubles = Arithmetic2[Double, ast.type, paradigm.type](paradigm)

  val realDoubles = RealArithmetic2[Double, ast.type, paradigm.type](paradigm)

  val ints = Arithmetic2[Int, ast.type, paradigm.type](paradigm)

  val strings = Strings2[ast.type, paradigm.type](paradigm)

  val equality = Equals2[ast.type, paradigm.type](paradigm)

  /*val consoleInMethod =
    new Console[MethodBodyCtxt, paradigm.type](
      paradigm, stringsInMethod
    )

  val consoleInConstructor =
    new Console[CtorCtxt, paradigm.type](
      paradigm, stringsInConstructor
    )
  
  val arraysInMethod =
    new Arrays[MethodBodyCtxt, paradigm.type](
      paradigm
    )

  val arraysInConstructor =
    new Arrays[CtorCtxt, paradigm.type](
      paradigm
    )
    */

  val listsInMethod = Lists2[ast.type, paradigm.type](paradigm)

  /*val listsInConstructor =
    Lists[CtorCtxt, paradigm.type, generics.type](
      paradigm,
      ooParadigm.constructorCapabilities.canGetMemberInConstructor,
      ooParadigm.constructorCapabilities.canApplyInConstructor,
      generics.constructorCapabilities.canApplyTypeInConstructor,
      ooParadigm.constructorCapabilities.canAddImportInConstructor
    )(generics)
  */
  
/*
  val treesInConstructor =
    Trees[CtorCtxt, paradigm.type, ObjectOriented](
      paradigm,
      ooParadigm.constructorCapabilities.canAddImportInConstructor
    )(ooParadigm)

  val assertionsInMethod = new Assertions[paradigm.type](paradigm)(ooParadigm)
  val exceptionsInMethod = new Exceptions[paradigm.type](paradigm)*/

  val assertionsInMethod = Assertions2[ast.type, paradigm.type](paradigm)

  
}

object CodeGenerator2 {

  case object Enable extends Command {
    type Result = Unit
  }


  def apply[AST <: FullAST](domainName: String, ast: AST): CodeGenerator2[ast.type] =
    new CodeGenerator2[ast.type](domainName, ast)
}
