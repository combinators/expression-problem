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
import org.combinators.ep.generator.FileWithPathPersistable._
import org.combinators.ep.generator.{FileWithPath, FileWithPathPersistable}
import org.combinators.ep.language.scala.codegen.CodeGenerator

import java.nio.file.{Path, Paths}

/**
 * Takes paradigm-independent specification for Fibonacci and generates Scala code
 */
class FibonacciRecursiveVariableMainScala {
  val generator = CodeGenerator("fibonacci")

  // functional
  val fibonacciApproach = FibonacciRecursiveVariableProvider[generator.syntax.type, generator.paradigm.type](generator.paradigm)(generator.nameProvider, generator.functional, generator.functionalControl, generator.ints, generator.assertionsInMethod, generator.equality)

  // imperative
  // val fibonacciApproach = FibonacciIndependentProvider.imperative[generator.syntax.type, generator.paradigm.type](generator.paradigm)(generator.nameProvider, generator.ooParadigm, generator.imperative, generator.ints, generator.assertionsInMethod, generator.equality)

  val persistable = FileWithPathPersistable[FileWithPath]

  def directToDiskTransaction(targetDirectory: Path): IO[Unit] = {

    val files =
      () => generator.paradigm.runGenerator {
        for {
          _ <- generator.ints.enable()
          _ <- generator.booleans.enable()
          _ <- generator.strings.enable()
          _ <- generator.equality.enable()
          _ <- generator.assertionsInMethod.enable()
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

object FibonacciRecursiveVariableScalaDirectToDiskMain extends IOApp {
  val targetDirectory = Paths.get("target", "fib", "scala")

  def run(args: List[String]): IO[ExitCode] = {

    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new FibonacciRecursiveVariableMainScala() }
      _ <- IO { println("[OK]") }
      result <- main.runDirectToDisc(targetDirectory)
    } yield result
  }
}
