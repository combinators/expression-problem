package org.combinators.ep.builder.scala.fibonacci

/**
 * sbt "helloWorld/runMain org.combinators.fibonacci.FibonacciScalaDirectToDiskMain"
 *
 * will generate the directory target/fib in which you can find a recursive implementation

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
import org.combinators.cogen.FileWithPathPersistable.*
import org.combinators.cogen.{FileWithPath, FileWithPathPersistable}
import org.combinators.ep.builder.inbetween.paradigm.ffi.TreesAST
import org.combinators.ep.language.scala.ScalaNameProvider
import org.combinators.ep.language.scala.ast.ffi.*
import org.combinators.ep.builder.inbetween.paradigm.ffi.Trees
import org.combinators.ep.language.scala.ast.{FinalBaseAST, FinalNameProviderAST}
import org.combinators.ep.language.scala.codegen.{CodeGenerator, CodeGenerator2, FullAST}
import org.combinators.ep.builder.scala.paradigm.ffi.*
import java.nio.file.{Path, Paths}

import org.combinators.fibonacci.FibonacciProvider

/**
 * Takes functional specification of Fibonacci with Lucas and generates Scala code.
 */
class FibonacciScala {
  val ast: FullAST & TreesAST = new FinalBaseAST
    with FinalNameProviderAST
    with FinalArithmeticAST
    with FinalAssertionsAST
    with FinalBooleanAST
    with FinalEqualsAST
    with FinalListsAST
    with FinalOperatorExpressionsAST
    with FinalRealArithmeticOpsAST
    with FinalStringsAST
    with FinalTreesAST {
    val reificationExtensions = List(scalaTreesOps.treeReificationExtensions)
  }

  val emptyset:Set[Seq[FibonacciScala.this.ast.any.Name]] = Set.empty
  val generator: CodeGenerator2[ast.type] = CodeGenerator2("fibonacci", ast, emptyset)

  //val generator = CodeGenerator("fibonacci")

  // TODO: Need to add generator.functional
  val fibonacciApproach = FibonacciProvider[generator.syntax.type, generator.paradigm.type](generator.paradigm)(generator.nameProvider, generator.functional, generator.functionalControl.functionalControlInMethods, generator.ints.arithmeticInMethods, generator.assertions.assertionsInMethods, generator.equality.equalsInMethods)

  val persistable: Aux[FileWithPath] = FileWithPathPersistable[FileWithPath]


  def directToDiskTransaction(targetDirectory: Path): IO[Unit] = {
    //FIX:
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

object FibonacciScalaDirectToDiskMain extends IOApp {
  val targetDirectory = Paths.get("target", "fib", "scala")

  def run(args: List[String]): IO[ExitCode] = {

    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new FibonacciScala() }
      _ <- IO { println("[OK]") }
      result <- main.runDirectToDisc(targetDirectory)
    } yield result
  }
}
