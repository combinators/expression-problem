package org.combinators.fibonacci

/**
 * sbt "helloWorld/runMain org.combinators.fibonacci.FibonacciIndependentScalaDirectToDiskMain"
 *
 * will generate the directory target/fib in which you can find the following implementation:

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
import org.combinators.ep.generator.FileWithPathPersistable._
import org.combinators.ep.generator.{FileWithPath, FileWithPathPersistable}
import org.combinators.ep.language.scala.codegen.CodeGenerator

import java.nio.file.{Path, Paths}

/**
 * Takes paradigm-independent specification for Fibonacci and generates Scala code
 */
class FibonacciMainScala {
  val generator = CodeGenerator("fibonacci")

  // functional
  val fibonacciApproach = FibonacciIndependentProvider.functional[generator.syntax.type, generator.paradigm.type](generator.paradigm)(generator.nameProvider, generator.functional, generator.functionalControl, generator.ints, generator.assertionsInMethod, generator.equality)

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

object FibonacciIndependentScalaDirectToDiskMain extends IOApp {
  val targetDirectory = Paths.get("target", "fib", "scala")

  def run(args: List[String]): IO[ExitCode] = {

    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new FibonacciMainScala() }
      _ <- IO { println("[OK]") }
      result <- main.runDirectToDisc(targetDirectory)
    } yield result
  }
}
