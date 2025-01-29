package org.combinators.ep.language.scala.codegen    /*DI:LD:AI*/

import cats.{Apply => _}
import org.combinators.ep.domain.GenericModel
import org.combinators.ep.generator.{Command, FileWithPath, Understands}
import org.combinators.ep.language.scala.{Finalized, ScalaNameProvider}
import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.generator
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.paradigm.{Apply, ToTargetLanguageType}
import org.combinators.ep.language.inbetween.any.{AbstractSyntax, CompilationUnit, AnyParadigm, Method, Name, Project, Type}
import org.combinators.ep.language.inbetween.oo.{Class, Constructor, OOParadigm}
import org.combinators.ep.language.inbetween.imperative.Imperative
import org.combinators.ep.language.inbetween.functional.{AlgebraicDataType, FunctionalParadigm}
import org.combinators.ep.language.inbetween.functional.control
import org.combinators.ep.language.inbetween.ffi.{Arithmetic, RealArithmetic, Booleans, Equals, Lists, Trees, Strings, Assertions}
import org.combinators.ep.language.inbetween.polymorphism.ParametricPolymorphism
import org.combinators.ep.language.inbetween.polymorphism.ParametricPolymorphismInADTContexts
import org.combinators.ep.language.inbetween.polymorphism.generics.Generics

import java.nio.file.{Path, Paths}

/**
 * Scala-specific.
 *
 * These paradigm-specific traits are conceptually different from each other
 */
sealed class CodeGenerator(domainName: String) { cc =>
  val factory = new Finalized.Factory {}


  val syntax: AbstractSyntax[Finalized.FinalTypes] = new AbstractSyntax[Finalized.FinalTypes] {}
  val nameProvider = new ScalaNameProvider[Finalized.FinalTypes](factory)

  def toLookup[Ctxt](name: String*): Option[Generator[Ctxt, Type[Finalized.FinalTypes]]] = {
    Some(Command.lift(factory.classReferenceType(name.map(nameProvider.mangle):_*)))
  }

  def addLookupsForImplementedGenerators[Ctxt](
    project: Project[Finalized.FinalTypes],
    add: (Project[Finalized.FinalTypes], TypeRep => Option[Generator[Ctxt, Type[Finalized.FinalTypes]]]) => Project[Finalized.FinalTypes]
  )(implicit canToTargetLanguage: Understands[Ctxt, ToTargetLanguageType[Type[Finalized.FinalTypes]]],
    canApplyType: Understands[Ctxt, Apply[Type[Finalized.FinalTypes], Type[Finalized.FinalTypes], Type[Finalized.FinalTypes]]]
  ): Project[Finalized.FinalTypes] = {
    add(project, {
      case TypeRep.Double => toLookup("Double")
      case TypeRep.Int => toLookup("Int")
      case TypeRep.Boolean => toLookup("Boolean")
      case TypeRep.String => toLookup("String")
      case TypeRep.Unit => toLookup("Unit")
      case TypeRep.Tree => toLookup("org", "combinators", "ep", "util", "Tree")
      case TypeRep.Sequence(elemTpeRep) =>
        Some(
          for {
            elemTpe <- ToTargetLanguageType[Type[Finalized.FinalTypes]](elemTpeRep).interpret(canToTargetLanguage)
            seqTpe <- Command.lift(factory.classReferenceType(nameProvider.mangle("Seq")))
            tpe <- Apply[
              Type[Finalized.FinalTypes],
              Type[Finalized.FinalTypes],
              Type[Finalized.FinalTypes]](seqTpe, Seq(elemTpe)).interpret(canApplyType)
          } yield tpe)
      case TypeRep.Arrow(src, tgt) => ??? // TODO: add new type constructor for A => B to newScala
      case _ => None
    })
  }

  def prefixExcludedTypes: Set[Seq[Name[Finalized.FinalTypes]]] = {
    Set(
      Seq("Double"),
      Seq("Boolean"),
      Seq("Int"),
      Seq("Unit"),
      Seq("String"),
      Seq("Seq"),
      Seq("org", "combinators", "ep", "util", "Tree"),
      Seq("org", "combinators", "ep", "util", "Node"),
      Seq("org", "combinators", "ep", "util", "Leaf")
    ).map(qname => qname.map(nameProvider.mangle))
  }

  def runGenerator(generator: Generator[Project[Finalized.FinalTypes], Unit]): Seq[FileWithPath] = {
    var projectWithLookups: Project[Finalized.FinalTypes] = factory.scalaProject(Set.empty)

    def buildFile: FileWithPath = {
      // create a rudimentary build.sbt for Scala just to work with sbt version 1.7.1
      val cmds = s"""
                    |scalaVersion := "3.3.3"
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
      addLookupsForImplementedGenerators[Method[Finalized.FinalTypes]](
        factory.convert(projectWithLookups),
        { case (project, lookup) => factory.convert(project).addTypeLookupsForMethods(lookup) }
      )(paradigm.methodBodyCapabilities.canTransformTypeInMethodBody,
        parametricPolymorphism.methodBodyCapabilities.canApplyTypeInMethod)
    projectWithLookups =
      addLookupsForImplementedGenerators[Method[Finalized.FinalTypes]](
        factory.convert(projectWithLookups),
        { case (project, lookup) => factory.convert(project).addTypeLookupsForFunctions(lookup) }
      )(paradigm.methodBodyCapabilities.canTransformTypeInMethodBody,
        parametricPolymorphism.methodBodyCapabilities.canApplyTypeInMethod)
    projectWithLookups =
      addLookupsForImplementedGenerators[Class[Finalized.FinalTypes]](
        factory.convert(projectWithLookups),
        { case (project, lookup) => factory.convert(project).addTypeLookupsForClasses(lookup) }
      )(ooParadigm.classCapabilities.canTranslateTypeInClass,
        generics.classCapabilities.canApplyTypeInClass)
    projectWithLookups =
      addLookupsForImplementedGenerators[Constructor[Finalized.FinalTypes]](
        factory.convert(projectWithLookups),
        { case (project, lookup) => factory.convert(project).addTypeLookupsForConstructors(lookup) }
      )(ooParadigm.constructorCapabilities.canTranslateTypeInConstructor,
        generics.constructorCapabilities.canApplyTypeInConstructor)
    projectWithLookups =
      addLookupsForImplementedGenerators[AlgebraicDataType[Finalized.FinalTypes]](
        factory.convert(projectWithLookups),
        { case (project, lookup) => factory.convert(project).addTypeLookupsForAlgebraicDataTypes(lookup) }
      )(functional.typeCapabilities.canTranslateTypeInType,
        parametricPolymorphismInADTContexts.algebraicDataTypeCapabilities.canApplyTypeInADT)


    val (generatedProject, _) = Command.runGenerator(generator, projectWithLookups)
    val withPrefix = factory.convert(generatedProject).prefixRootPackage(Seq(nameProvider.mangle(domainName)), prefixExcludedTypes)


    def toFileWithPath(cu: CompilationUnit[Finalized.FinalTypes], basePath: Path): FileWithPath = {
      FileWithPath(factory.convert(cu).toScala, {
        val nameAsStrings = cu.name.map(name => factory.convert(name).toScala)
        val nameWithScalaExtension = nameAsStrings.init :+ (nameAsStrings.last + ".scala")
        nameWithScalaExtension.foldLeft(basePath)({ case (path, name) =>
          Paths.get(path.toString, name)
        })
      })
    }
    val mainDir = Paths.get("src", "main", "scala")
    val testDir = Paths.get("src", "test", "scala")
    withPrefix.compilationUnits.flatMap(cu => {
      import factory._
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
    }).toSeq :+ treeLibrary :+ buildFile :+ pluginsFile :+ scalaFmt
  }

  val paradigm = AnyParadigm[Finalized.FinalTypes, factory.type, syntax.type](factory, runGenerator, syntax)
  val ooParadigm = OOParadigm[Finalized.FinalTypes, factory.type, paradigm.type](paradigm)
  val imperative = Imperative[Finalized.FinalTypes, factory.type, paradigm.type](paradigm)
  val functional = FunctionalParadigm[Finalized.FinalTypes, factory.type, paradigm.type](paradigm)
  val functionalControl = control.Functional[Finalized.FinalTypes, factory.type, paradigm.type](paradigm)

  val parametricPolymorphism = ParametricPolymorphism[Finalized.FinalTypes, factory.type, paradigm.type](paradigm)
  val generics = Generics[Finalized.FinalTypes, factory.type, paradigm.type](paradigm)(ooParadigm, parametricPolymorphism)
  val parametricPolymorphismInADTContexts = ParametricPolymorphismInADTContexts[Finalized.FinalTypes, factory.type, paradigm.type](paradigm)(functional)


  val booleans = Booleans[Finalized.FinalTypes, factory.type, paradigm.type](paradigm)

  val doubles = Arithmetic[Finalized.FinalTypes, factory.type, paradigm.type, Double](paradigm)

  val realDoubles = RealArithmetic[Finalized.FinalTypes, factory.type, paradigm.type, Double](paradigm)

  val ints = Arithmetic[Finalized.FinalTypes, factory.type, paradigm.type, Int](paradigm)

  val strings = Strings[Finalized.FinalTypes, factory.type, paradigm.type](paradigm)

  val equality = Equals[Finalized.FinalTypes, factory.type, paradigm.type](paradigm)

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

  val listsInMethod = Lists[Finalized.FinalTypes, factory.type, paradigm.type](paradigm)

  /*val listsInConstructor =
    Lists[CtorCtxt, paradigm.type, generics.type](
      paradigm,
      ooParadigm.constructorCapabilities.canGetMemberInConstructor,
      ooParadigm.constructorCapabilities.canApplyInConstructor,
      generics.constructorCapabilities.canApplyTypeInConstructor,
      ooParadigm.constructorCapabilities.canAddImportInConstructor
    )(generics)
  */
  val treesInMethod =
    Trees[Finalized.FinalTypes, factory.type, paradigm.type](paradigm)(Map.empty)
/*
  val treesInConstructor =
    Trees[CtorCtxt, paradigm.type, ObjectOriented](
      paradigm,
      ooParadigm.constructorCapabilities.canAddImportInConstructor
    )(ooParadigm)

  val assertionsInMethod = new Assertions[paradigm.type](paradigm)(ooParadigm)
  val exceptionsInMethod = new Exceptions[paradigm.type](paradigm)*/

  val assertionsInMethod = Assertions[Finalized.FinalTypes, factory.type, paradigm.type](paradigm)

  def treeLibrary: FileWithPath = {
    FileWithPath(
      getClass.getResourceAsStream("/scala-code/org/combinators/ep/util/Trees.scala").readAllBytes(),
      Paths.get("src", "main", "scala", "org", "combinators", "ep", "util", "Trees.scala")
    )
  }
}

object CodeGenerator {

  case object Enable extends Command {
    type Result = Unit
  }


  def apply(domainName: String): CodeGenerator =
    new CodeGenerator(domainName)
}
