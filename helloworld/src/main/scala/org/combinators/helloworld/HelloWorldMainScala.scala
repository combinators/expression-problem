package org.combinators.helloworld

/**
 * sbt "helloWorld/runMain org.combinators.helloworld.HelloWorldJavaDirectToDiskMain"
 *
 * Creates output files in target/helloworld
 */

import cats.effect.{ExitCode, IO, IOApp}
import com.github.javaparser.ast.PackageDeclaration
import org.apache.commons.io.FileUtils
import org.combinators.cogen.{FileWithPath, FileWithPathPersistable}
import org.combinators.ep.language.scala.codegen.FullAST
import FileWithPathPersistable._
import org.combinators.ep.language.scala.ast._
import org.combinators.ep.language.scala.ast.ffi._
import org.combinators.ep.language.scala.ast.{FinalBaseAST, FinalNameProviderAST}
import org.combinators.ep.language.scala.codegen.CodeGenerator
import java.nio.file.{Path, Paths}

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 */
class HelloWorldMainScala {
  val _ast: FullAST = new FinalBaseAST
    with FinalNameProviderAST
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
    with FinalStringAST {
    val reificationExtensions = List(scalaMapsOps.mapReificationExtensions)
  }
  val generator: CodeGenerator[_ast.type] = CodeGenerator("dp", _ast, Set.empty)

  val helloWorldApproach = HelloWorldObjectOrientedProvider[generator.syntax.type, generator.paradigm.type](generator.paradigm)(generator.nameProvider, generator.imperative.imperativeInMethods, generator.ooParadigm, generator.ints.arithmeticInMethods, generator.console.consoleInMethods, generator.arrays.arraysInMethods, generator.assertions.assertionsInMethods, generator.equality.equalsInMethods, generator.maps.mapsInMethods)

  val persistable: Aux[FileWithPath] = FileWithPathPersistable[FileWithPath]

  def directToDiskTransaction(targetDirectory: Path): IO[Unit] = {

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
          
          _ <- helloWorldApproach.implement()
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

  def runDirectToDisc(targetDirectory: Path): IO[ExitCode] = {
    for {
      _ <- directToDiskTransaction(targetDirectory)
    } yield ExitCode.Success
  }
}

object HelloWorldScalaDirectToDiskMain extends IOApp {
  val targetDirectory = Paths.get("target", "helloworld")

  def run(args: List[String]): IO[ExitCode] = {
    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new HelloWorldMainScala() }
      _ <- IO { println("[OK]") }
      result <- main.runDirectToDisc(targetDirectory)
    } yield result
  }
}
