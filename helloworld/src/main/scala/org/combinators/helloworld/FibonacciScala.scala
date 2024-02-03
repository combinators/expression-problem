package org.combinators.helloworld

/* Generates Fibonacci Program. */

import cats.effect.{ExitCode, IO, IOApp}
import org.apache.commons.io.FileUtils
import org.combinators.ep.generator.FileWithPathPersistable._
import org.combinators.ep.generator.{FileWithPath, FileWithPathPersistable}
//FIX: import org.combinators.ep.language.scala.ScalaNameProvider
//FIX: import org.combinators.ep.language.scala.codegen.CodeGenerator

import java.nio.file.{Path, Paths}
import scala.meta.prettyprinters.Syntax
import scala.meta.{Pkg, Term}

/**
 * Takes functional specification of Fibonacci with Lucas and generates Scala code.
 */
class FibonacciScala {
  //FIX:   val generator = CodeGenerator(CodeGenerator.defaultConfig.copy(targetPackage = Pkg(Term.Name("fib"), List.empty)))

  //FIX:  val fibonacciApproach = FibonacciProvider[Syntax.default.type, generator.paradigm.type](generator.paradigm)(ScalaNameProvider, generator.functional, generator.functionalInMethod, generator.intsInMethod, generator.assertionsInMethod, generator.equalityInMethod)

  val persistable = FileWithPathPersistable[FileWithPath]

  def directToDiskTransaction(targetDirectory: Path): IO[Unit] = {
    //FIX:
//    val files =
//      () => generator.paradigm.runGenerator {
//        for {
//          _ <- generator.intsInMethod.enable()
//          _ <- generator.booleansInMethod.enable()
//          _ <- generator.stringsInMethod.enable()
//          _ <- generator.equalityInMethod.enable()
//          _ <- generator.assertionsInMethod.enable()
//          _ <- fibonacciApproach.make_project()
//        } yield ()
//      }

     IO {
      print("Computing Files...")
       //FIX:     val computed = files()
      println("[OK]")
      if (targetDirectory.toFile.exists()) {
        print(s"Cleaning Target Directory (${targetDirectory})...")
        FileUtils.deleteDirectory(targetDirectory.toFile)
        println("[OK]")
      }
      print("Persisting Files...")
       //FIX:    files().foreach(file => persistable.persistOverwriting(targetDirectory, file))
      println("[OK]")
    }
  }

  def runDirectToDisc(targetDirectory: Path): IO[ExitCode] = {
    for {
      _ <- directToDiskTransaction(targetDirectory)
    } yield ExitCode.Success
  }
}

object FibonacciScalaDirectToDiskMain extends IOApp {
  val targetDirectory = Paths.get("target", "ep3", "scala")

  def run(args: List[String]): IO[ExitCode] = {

    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new FibonacciScala() }
      _ <- IO { println("[OK]") }
      result <- main.runDirectToDisc(targetDirectory)
    } yield result
  }
}
