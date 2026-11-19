package org.combinators.fibonacci

/**
 * sbt "helloWorld/runMain org.combinators.fibonacci.FibonacciRecursiveVariableMainScalaDirectToDiskMain"
 *
 * will generate the directory target/fib in which you can find a recursive functional implementation:

        package fibonacci
        def fib(n: Int): Int = {
          return {
            {
              def inner_loop: Function[Int, Int] = (n: Int) => {
                if ((n <= 1)) {
                  n
                } else {
                  (inner_loop((n - 1)) + inner_loop((n - 2)))
                }
              }
              inner_loop(n)
            }
          }
        }
 *
 *  sbt "helloWorld/runMain org.combinators.fibonacci.FibonacciRecursiveVariableMainScalaDirectToDiskMain imperative"
 *
 *  will generate the directory target/fib in which you can find a recursive functional implementation:

    package fibonacci
    class Fib {
      def fib(n: Int): Int = {
        if ((n <= 1)) {
          return { n }
        } else {
          return { (this.fib((n - 1)) + this.fib((n - 2))) }
        }
      }
    }

 */

import cats.effect.{ExitCode, IO, IOApp}
import org.apache.commons.io.FileUtils
import org.combinators.cogen.FileWithPathPersistable.*
import org.combinators.cogen.{FileWithPath, FileWithPathPersistable}
import org.combinators.ep.language.scala.ast.ffi.*
import org.combinators.ep.language.scala.ast.{FinalBaseAST, FinalNameProviderAST}
import org.combinators.ep.language.scala.codegen.{CodeGenerator, FullAST}
import org.combinators.fibonacci.FibonacciRecursiveVariableProvider

import java.nio.file.{Path, Paths}

// different approaches
val FUNCTIONAL = "functional"
val IMPERATIVE = "imperative"

/**
 * Takes paradigm-independent specification for Fibonacci and generates Scala code
 */
class FibonacciRecursiveVariableMainScala {
  val ast: FullAST = new FinalBaseAST
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

  val emptyset: Set[Seq[FibonacciRecursiveVariableMainScala.this.ast.any.Name]] = Set.empty
  val generator: CodeGenerator[ast.type] = CodeGenerator("fibonacci", ast, emptyset)

  // functional
  val fibonacciApproachFunctional = FibonacciRecursiveVariableProvider[generator.syntax.type, generator.paradigm.type](generator.paradigm)(generator.nameProvider, generator.functional, generator.functionalControl.functionalControlInMethods, generator.ints.arithmeticInMethods, generator.assertions.assertionsInMethods, generator.equality.equalsInMethods)

  // imperative
  val fibonacciApproachImperative = FibonacciIndependentProvider.imperative[generator.syntax.type, generator.paradigm.type](generator.paradigm)(generator.nameProvider, generator.ooParadigm, generator.imperative.imperativeInMethods, generator.ints.arithmeticInMethods, generator.assertions.assertionsInMethods, generator.equality.equalsInMethods)

  val persistable: Aux[FileWithPath] = FileWithPathPersistable[FileWithPath]

  def directToDiskTransaction(which:String, targetDirectory: Path): IO[Unit] = {

    val files =
      () => generator.paradigm.runGenerator {
        for {
          _ <- generator.enableDefaultFFIs()
          _ <- if (which.equals(FUNCTIONAL)) {
            fibonacciApproachFunctional.make_project()
          } else {
            fibonacciApproachImperative.make_project()
          }
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

  def runDirectToDisc(which:String, targetDirectory: Path): IO[ExitCode] = {
    for {
      _ <- directToDiskTransaction(which, targetDirectory)
    } yield ExitCode.Success
  }
}

object FibonacciRecursiveVariableMainScalaDirectToDiskMain extends IOApp {
  private val targetDirectory = Paths.get("target", "fib", "scala")

  def run(args: List[String]): IO[ExitCode] = {

    // default to functional
    val which = if (args.length > 0) {
      if (args.head.toLowerCase() == FUNCTIONAL.toLowerCase()) {
        FUNCTIONAL
      } else if (args.head.toLowerCase() == IMPERATIVE.toLowerCase()) {
        IMPERATIVE
      } else {
        throw new IllegalArgumentException(s"Argument must be either ${FUNCTIONAL} or ${IMPERATIVE}")
      }
    } else {
      FUNCTIONAL
    }

    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new FibonacciRecursiveVariableMainScala() }
      _ <- IO { println("[OK]") }
      result <- main.runDirectToDisc(which, targetDirectory)
    } yield result
  }
}
