package org.combinators.fibonacci

/* Generates Fibonacci Program. */

import cats.effect.{ExitCode, IO, IOApp}
import org.apache.commons.io.FileUtils
import org.combinators.ep.generator.FileWithPathPersistable._
import org.combinators.ep.generator.{FileWithPath, FileWithPathPersistable}
import org.combinators.ep.language.scala.codegen.CodeGenerator
//FIX: import org.combinators.ep.language.java.AlternateMain.generator
//FIX: import org.combinators.ep.language.scala.{CodeGenerator, ScalaNameProvider, Syntax}

import java.nio.file.{Path, Paths}

/**
 * Takes functional specification of Fibonacci with Lucas and generates Scala code.
 */
class FibonacciMainWithLucasScala {
  val generator = CodeGenerator("fibonacci")
  val fibonacciApproach = FibonacciWithLucasProvider[generator.syntax.type, generator.paradigm.type](generator.paradigm)(generator.nameProvider, generator.functional, generator.functionalControl, generator.ints, generator.assertionsInMethod, generator.equality)

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

object FibonacciWithLucasScalaDirectToDiskMain extends IOApp {
  val targetDirectory:Path = Paths.get("target", "ep3", "scala")

  // generated!
  def fib(n: Int): Int = {
    if (n <= 0) 0 else if (n <= 1) 1 else if (n <= 2) 1 else if (n <= 3) 2 else (fib(n / 2) * lucas(n - n / 2) + lucas(n / 2) * fib(n - n / 2)) / 2
  }

  def lucas(n: Int): Int = {
    if (n <= 0) 2 else if (n <= 1) 1 else fib(n - 1) + fib(n + 1)
  }

  def run(args: List[String]): IO[ExitCode] = {
    val t = fib(20)
    println(t)
    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new FibonacciMainWithLucasScala() }
      _ <- IO { println("[OK]") }
      result <- main.runDirectToDisc(targetDirectory)
    } yield result
  }
}
