package org.combinators.fibonacci

/**

    package fibonacci
    def fib(n: Int): Int = {
      return {
        if ((n <= 1)) {
          n
        } else {
          (fibonacci.fib((n - 1)) + fibonacci.fib((n - 2)))
        }
      }
    }

 */

import cats.effect.{ExitCode, IO, IOApp}
import org.apache.commons.io.FileUtils
import org.combinators.cogen.{FileWithPath, FileWithPathPersistable}
import org.combinators.ep.language.scala.codegen.FullAST
import FileWithPathPersistable._
import org.combinators.ep.language.scala.ast.ffi._
import org.combinators.ep.language.scala.ast.{FinalBaseAST, FinalNameProviderAST}
import org.combinators.ep.language.scala.codegen.CodeGenerator
import java.nio.file.{Path, Paths}

/**
 * Takes paradigm-independent specification for Fibonacci and generates Scala code
 */
class FibonacciMainScala {
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
    with FinalStringAST
    with FinalUnitAST {
    val reificationExtensions = List.empty
  }

  val emptyset: Set[Seq[_ast.any.Name]] = Set.empty
  val generator: CodeGenerator[_ast.type] = CodeGenerator("fibonacci", _ast, emptyset)

  // functional
  val fibonacciApproach = FibonacciIndependentProvider.functional[generator.syntax.type, generator.paradigm.type](generator.paradigm)(generator.nameProvider, generator.functional, generator.functionalControl.functionalControlInMethods, generator.ints.arithmeticInMethods, generator.assertions.assertionsInMethods, generator.equality.equalsInMethods)

  val persistable: Aux[FileWithPath] = FileWithPathPersistable[FileWithPath]

  def directToDiskTransaction(targetDirectory: Path): IO[Unit] = {

    val files =
      () => generator.paradigm.runGenerator {
        for {
          _ <- generator.enableDefaultFFIs()

          _ <- fibonacciApproach.make_project()
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
      computed.foreach(file => persistable.persistOverwriting(targetDirectory, file))
      println("[OK]")
    }
  }

  def runDirectToDisc(targetDirectory: Path): IO[ExitCode] = {
    for {
      _ <- directToDiskTransaction(targetDirectory)
    } yield ExitCode.Success
  }
}

object FibonacciMainScalaDirectToDiskMain extends IOApp {
  private val targetDirectory = Paths.get("target", "fib", "scala")

  def run(args: List[String]): IO[ExitCode] = {

    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new FibonacciMainScala() }
      _ <- IO { println("[OK]") }
      result <- main.runDirectToDisc(targetDirectory)
    } yield result
  }
}
