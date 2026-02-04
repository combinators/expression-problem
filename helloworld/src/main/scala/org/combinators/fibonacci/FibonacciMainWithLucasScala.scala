package org.combinators.fibonacci

/**
 * sbt "helloWorld/runMain org.combinators.fibonacci.FibonacciWithLucasScalaDirectToDiskMain"
 *
 * will generate the directory target/fib in which you can find a mutual-recursive implementation
 * that avoids the standard issues that a recursive fib(n) implementation would have by taking
 * advantage of an identity with the Lucas Numbers:
 *
 *   Lucas(n) = Fib(n-1) + F(n+1) for n > 1
 *   Fib(x+y) = [(Fib(x)*Lucas(y) + Fib(y)*Locas(x)] / 2

          package fibonacci
          def fib(n: Int): Int = {

            return {
              if ((n <= 0)) {
                0
              } else {
                if ((n <= 1)) {
                  1
                } else {
                  if ((n <= 2)) {
                    1
                  } else {
                    if ((n <= 3)) {
                      2
                    } else {
                      (((fibonacci.fib((n / 2)) * fibonacci.lucas(
                        (n - (n / 2))
                      )) + (fibonacci.lucas((n / 2)) * fibonacci.fib((n - (n / 2))))) / 2)
                    }
                  }
                }
              }
            }
          }

          def lucas(n: Int): Int = {
            return {
              if ((n <= 0)) {
                2
              } else {
                if ((n <= 1)) {
                  1
                } else {
                  (fibonacci.fib((n - 1)) + fibonacci.fib((n + 1)))
                }
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
import org.combinators.fibonacci.FibonacciWithLucasProvider

import java.nio.file.{Path, Paths}

/**
 * Takes functional specification of Fibonacci with Lucas and generates Scala code.
 */
class FibonacciMainWithLucasScala {
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

  val emptyset: Set[Seq[FibonacciMainWithLucasScala.this.ast.any.Name]] = Set.empty
  val generator: CodeGenerator[ast.type] = CodeGenerator("fibonacci", ast, emptyset)

  val fibonacciApproach = FibonacciWithLucasProvider[generator.syntax.type, generator.paradigm.type](generator.paradigm)(generator.nameProvider, generator.functional, generator.functionalControl.functionalControlInMethods, generator.ints.arithmeticInMethods, generator.assertions.assertionsInMethods, generator.equality.equalsInMethods)

  //[generator.syntax.type, generator.paradigm.type](generator.paradigm)(generator.nameProvider, generator.functional, generator.functionalControl, generator.ints, generator.assertionsInMethod, generator.equality)

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

object FibonacciMainWithLucasScalaDirectToDiskMain extends IOApp {
  private val targetDirectory:Path = Paths.get("target", "fib", "scala")

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
