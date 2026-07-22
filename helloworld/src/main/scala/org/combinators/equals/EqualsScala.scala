package org.combinators.equals


import cats.effect.{ExitCode, IO, IOApp}
import com.github.javaparser.ast.PackageDeclaration
import org.apache.commons.io.FileUtils
import org.combinators.cogen.{Command, FileWithPath, FileWithPathPersistable, TypeRep}
import org.combinators.ep.language.scala.codegen.FullAST
import FileWithPathPersistable.*
import org.combinators.ep.language.scala.ast.ffi.*
import org.combinators.ep.language.scala.ast.{BaseAST, FinalBaseAST, FinalNameProviderAST, NameProviderAST}
import org.combinators.ep.language.scala.codegen.CodeGenerator
import org.combinators.equals.ffi.BaseType.CompositeTpe
import org.combinators.equals.ffi.scala.FinalBaseTypeAST

import java.nio.file.{Path, Paths}

// CoCo strategy works as follows
//   1. Override functionality as desired with Trait
//   2. Final Factories


trait NameProviderAllowsEquals extends NameProviderAST { self: BaseAST =>

  object nameProviderAllowsEquals {
    object nameProviderAllowsEqualsOverrides {
      trait FinalTypes extends nameProvider.FinalTypes {
        type NameProvider <: NameProviderAllowsEquals
      }

      trait NameProviderAllowsEquals extends nameProvider.ScalaNameProvider {

        // allow equals
        override def mangle(name: String): any.Name = {
          mangle(name, Set("Object", "hashCode", "toString", "getClass", "type"))
        }
      }

      trait Factory extends nameProvider.Factory {
        // no impact on Factory, so this remains
      }
    }
  }

  val nameProviderFinalTypes: nameProviderAllowsEquals.nameProviderAllowsEqualsOverrides.FinalTypes
  val nameProviderFactory: nameProviderAllowsEquals.nameProviderAllowsEqualsOverrides.Factory
}

trait FinalNameProviderAllowsEqualsAST extends NameProviderAllowsEquals { self: BaseAST =>
  object finalNameProviderAllowsEqualsFinalTypes {
    trait NameProviderFinalTypes extends nameProviderAllowsEquals.nameProviderAllowsEqualsOverrides.FinalTypes {
      type NameProvider = nameProviderAllowsEquals.nameProviderAllowsEqualsOverrides.NameProviderAllowsEquals
    }
  }

  object finalNameProviderAllowsEqualsFactoryTypes {
    trait NameProviderFactory extends nameProviderAllowsEquals.nameProviderAllowsEqualsOverrides.Factory {
      def scalaNameProvider: nameProviderAllowsEquals.nameProviderAllowsEqualsOverrides.NameProviderAllowsEquals = {
        case class ScalaNameProvider() extends nameProviderAllowsEquals.nameProviderAllowsEqualsOverrides.NameProviderAllowsEquals {
          override def getSelfNameProvider: nameProviderAllowsEquals.nameProviderAllowsEqualsOverrides.NameProviderAllowsEquals = this
        }
        ScalaNameProvider()
      }
    }
  }

  override val nameProviderFinalTypes: finalNameProviderAllowsEqualsFinalTypes.NameProviderFinalTypes = new finalNameProviderAllowsEqualsFinalTypes.NameProviderFinalTypes {}
  override val nameProviderFactory: finalNameProviderAllowsEqualsFactoryTypes.NameProviderFactory = new finalNameProviderAllowsEqualsFactoryTypes.NameProviderFactory {}
}

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 */
class EqualsMainScala {
  val _ast: FullAST & FinalBaseTypeAST = new FinalBaseAST
    with FinalNameProviderAllowsEqualsAST  // FinalNameProviderAST has been replaced by CoCo
    with FinalArithmeticAST
    with FinalArraysAST
    with FinalAssertionsAST
    with FinalBooleanAST
    with FinalConsoleAST
    with FinalExceptionsAST
    with FinalEqualsAST
    with FinalListsAST
    with FinalMapsAST
    with FinalOperatorExpressionsAST
    with FinalRealArithmeticOpsAST
    with FinalBaseTypeAST
    with FinalStringAST {
    val reificationExtensions = List.empty
  }
  val generator: CodeGenerator[_ast.type] = CodeGenerator("eql", _ast, _ast.scalaBaseTypeOps.baseTypePrefixExcludes)

  val baseTypeIn: org.combinators.equals.ffi.inbetween.BaseType.WithBase[_ast.type, generator.paradigm.type, _ast.any.Method] = {
    import _ast.factory._

    org.combinators.equals.ffi.inbetween.BaseType[_ast.type, generator.paradigm.type, _ast.any.Method](generator.paradigm)( (project,tpeLookup) => {
      project
        .addTypeLookupsForMethods(tpeRep => tpeLookup(tpeRep).map(Command.lift))
        .addTypeLookupsForClasses(tpeRep => tpeLookup(tpeRep).map(Command.lift))
        .addTypeLookupsForConstructors(tpeRep => tpeLookup(tpeRep).map(Command.lift))
        .addTypeLookupsForFunctions(tpeRep => tpeLookup(tpeRep).map(Command.lift))
        .addTypeLookupsForAlgebraicDataTypes(tpeRep => tpeLookup(tpeRep).map(Command.lift))
    })
  }

  val eqlsGenerator: EqualsGenerator.Aux[generator.paradigm.type, generator.ooParadigm.type] = EqualsGenerator(generator.paradigm)(
    generator.nameProvider, baseTypeIn, generator.imperative.imperativeInMethods, generator.equality.equalsInMethods, generator.booleans.booleansInMethodsInMethods, generator.arrays.arraysInMethods, generator.ooParadigm)

  val equalsApproach = EqualsObjectOrientedProvider[generator.syntax.type, generator.paradigm.type](generator.paradigm)(generator.nameProvider, generator.imperative.imperativeInMethods, generator.ooParadigm, generator.ints.arithmeticInMethods, generator.booleans.booleansInMethodsInMethods, generator.console.consoleInMethods, generator.arrays.arraysInMethods, generator.assertions.assertionsInMethods, generator.equality.equalsInMethods, generator.maps.mapsInMethods, baseTypeIn, eqlsGenerator)

  val persistable: Aux[FileWithPath] = FileWithPathPersistable[FileWithPath]

  def directToDiskTransaction(targetDirectory: Path, domains:Seq[CompositeDataType]): IO[Unit] = {

    val files =
      () => generator.paradigm.runGenerator {
        for {
          _ <- generator.doubles.arithmeticInMethods.enable()
          _ <- generator.ints.arithmeticInMethods.enable()
          _ <- generator.strings.stringsInMethods.enable()
          _ <- generator.lists.listsInMethods.enable()   
          _ <- generator.console.consoleInMethods.enable()
          _ <- generator.arrays.arraysInMethods.enable()
          _ <- generator.equality.equalsInMethods.enable()
          _ <- generator.assertions.assertionsInMethods.enable()
          _ <- generator.maps.mapsInMethods.enable()
          _ <- generator.booleans.booleansInMethodsInMethods.enable()

          _ <- baseTypeIn.enable()
          _ <- equalsApproach.implement(domains)
        } yield ()
      }

     IO {
      print("Computing Files...")
      val computed = files()
      println("[OK]")
      if (targetDirectory.toFile.exists()) {
        print(s"Cleaning Target Directory ($targetDirectory)...")
        FileUtils.deleteDirectory(targetDirectory.toFile)
        println("[OK]")
      }
      print("Persisting Files...")
      files().foreach(file => persistable.persistOverwriting(targetDirectory, file))
      println("[OK]")
    }
  }

  def runDirectToDisc(targetDirectory: Path, domains:Seq[CompositeDataType]): IO[ExitCode] = {
    for {
      _ <- directToDiskTransaction(targetDirectory, domains)
    } yield ExitCode.Success
  }
}

object EqualsScalaDirectToDiskMain extends IOApp {
  val targetDirectory = Paths.get("target", "eql")

  val point: CompositeDataType = new CompositeDataType(
    name = "Point",
    fields = Map("x" -> BuiltInDataType(TypeRep.Int), "y" -> BuiltInDataType(TypeRep.Int))
  )

  // Not yet ready for "previous" -> ArrayDataType(TypeRep.Int))
  val pointTypeRep: TypeRep.OfHostType[Map[String, Any]] = CompositeTpe(point)
  val abc = TypeRep.Array[Map[String, Any]](CompositeTpe(point))
  val domain: CompositeDataType = new CompositeDataType(
    name = "Rectangle",
    fields = Map("height" -> BuiltInDataType(TypeRep.Int), "width" -> BuiltInDataType(TypeRep.Int),
      "anchor" -> BuiltInDataType(abc))
  )

  def run(args: List[String]): IO[ExitCode] = {
    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new EqualsMainScala() }
      _ <- IO { println("[OK]") }
      result <- main.runDirectToDisc(targetDirectory, Seq(domain, point))
    } yield result
  }
}
