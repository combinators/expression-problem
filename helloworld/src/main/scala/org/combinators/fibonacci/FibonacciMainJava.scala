package org.combinators.fibonacci

/**
 * sbt "helloWorld/runMain org.combinators.fibonacci.FibonacciJavaDirectToDiskMain"
 *
 * will generate the directory target/fib in which you can find following recursive implementation

      package fibonacci;
      public class Fib {
          public Integer fib(Integer n) {
              if ((n <= 1)) {
                  return n;
              } else {
                  return (this.fib((n - 1)) + this.fib((n - 2)));
              }
          }
      }

 */
import cats.effect.{ExitCode, IO, IOApp}
import com.github.javaparser.ast.PackageDeclaration
import org.apache.commons.io.FileUtils
import org.combinators.ep.generator.FileWithPathPersistable._
import org.combinators.ep.generator.{FileWithPath, FileWithPathPersistable}
import org.combinators.ep.language.java.paradigm.ObjectOriented
import org.combinators.ep.language.java.{CodeGenerator, JavaNameProvider, PartiallyBoxed, Syntax}

import java.nio.file.{Path, Paths}

/**
 * Takes paradigm-independent specification for Fibonacci and generates Java code
 */
class FibonacciMainJava {
  val generator = CodeGenerator(CodeGenerator.defaultConfig.copy(boxLevel = PartiallyBoxed, targetPackage = new PackageDeclaration(ObjectOriented.fromComponents("fibonacci"))))

  val fibonacciApproach = FibonacciIndependentProvider.imperative[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm, generator.imperativeInMethod, generator.intsInMethod, generator.assertionsInMethod, generator.equalityInMethod)

  val persistable = FileWithPathPersistable[FileWithPath]

  def directToDiskTransaction(targetDirectory: Path): IO[Unit] = {

    val files =
      () => generator.paradigm.runGenerator {
        for {
          _ <- generator.intsInMethod.enable()
          _ <- generator.booleansInMethod.enable()
          _ <- generator.stringsInMethod.enable()
          _ <- generator.equalityInMethod.enable()
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

object FibonacciJavaDirectToDiskMain extends IOApp {
  val targetDirectory = Paths.get("target", "fib", "java")

  def run(args: List[String]): IO[ExitCode] = {
    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new FibonacciMainJava() }
      _ <- IO { println("[OK]") }
      result <- main.runDirectToDisc(targetDirectory)
    } yield result
  }
}
