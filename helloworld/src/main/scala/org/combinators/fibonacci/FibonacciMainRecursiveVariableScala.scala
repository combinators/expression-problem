package org.combinators.fibonacci

/**
 * sbt "helloWorld/runMain org.combinators.fibonacci.FibonacciRecursiveVariableScalaDirectToDiskMain"
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

/**
 * Takes paradigm-independent specification for Fibonacci and generates Scala code
 */
class FibonacciRecursiveVariableMainScala {
  val ast: FullAST = new FinalBaseAST
    with FinalNameProviderAST
    with FinalArithmeticAST
    with FinalAssertionsAST
    with FinalBooleanAST
    with FinalEqualsAST
    with FinalListsAST
    with FinalOperatorExpressionsAST
    with FinalRealArithmeticOpsAST
    with FinalStringAST {
    val reificationExtensions = List.empty
  }

  val emptyset: Set[Seq[FibonacciRecursiveVariableMainScala.this.ast.any.Name]] = Set.empty
  val generator: CodeGenerator[ast.type] = CodeGenerator("fibonacci", ast, emptyset)

  // functional
  val fibonacciApproach = FibonacciRecursiveVariableProvider[generator.syntax.type, generator.paradigm.type](generator.paradigm)(generator.nameProvider, generator.functional, generator.functionalControl.functionalControlInMethods, generator.ints.arithmeticInMethods, generator.assertions.assertionsInMethods, generator.equality.equalsInMethods)

  //[generator.syntax.type, generator.paradigm.type](generator.paradigm)(generator.nameProvider, generator.functional, generator.functionalControl, generator.ints, generator.assertionsInMethod, generator.equality)

  // imperative
  // val fibonacciApproach = FibonacciIndependentProvider.imperative[generator.syntax.type, generator.paradigm.type](generator.paradigm)(generator.nameProvider, generator.ooParadigm, generator.imperative, generator.ints, generator.assertionsInMethod, generator.equality)

  val persistable: Aux[FileWithPath] = FileWithPathPersistable[FileWithPath]

  def directToDiskTransaction(targetDirectory: Path): IO[Unit] = {

    val files =
      () => generator.paradigm.runGenerator {
        for {
          _ <- generator.ints.arithmeticInMethods.enable()
          _ <- generator.booleans.booleansInMethodsInMethods.enable()
          _ <- generator.strings.stringsInMethods.enable()
          _ <- generator.equality.equalsInMethods.enable()
          _ <- generator.assertions.assertionsInMethods.enable()

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

object FibonacciRecursiveVariableMainScalaDirectToDiskMain extends IOApp {
  private val targetDirectory = Paths.get("target", "fib", "scala")

  def run(args: List[String]): IO[ExitCode] = {

    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new FibonacciRecursiveVariableMainScala() }
      _ <- IO { println("[OK]") }
      result <- main.runDirectToDisc(targetDirectory)
    } yield result
  }
}
