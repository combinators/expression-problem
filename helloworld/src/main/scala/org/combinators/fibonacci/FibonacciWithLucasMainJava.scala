package org.combinators.fibonacci

/**
 * sbt "helloWorld/runMain org.combinators.fibonacci.FibonacciWithLucasJavaDirectToDiskMain"
 *
 * will generate the directory target/fib in which you can find a mutual-recursive implementation
 * that avoids the standard issues that a recursive fib(n) implementation would have by taking
 * advantage of an identity with the Lucas Numbers:
 *
 *   Lucas(n) = Fib(n-1) + F(n+1) for n > 1
 *   Fib(x+y) = [(Fib(x)*Lucas(y) + Fib(y)*Locas(x)] / 2

    ackage fibonacci;

    public class Fib {

        public Integer lucas(Integer n) {
            if ((n <= 0)) {
                return 2;
            } else if ((n <= 1)) {
                return 1;
            } else {
                return (this.fib((n - 1)) + this.fib((n + 1)));
            }
        }

        public Integer fib(Integer n) {
            if ((n <= 0)) {
                return 0;
            } else if ((n <= 1)) {
                return 1;
            } else if ((n <= 2)) {
                return 1;
            } else if ((n <= 3)) {
                return 2;
            } else {
                return (((this.fib((n / 2)) * this.lucas((n - (n / 2)))) + (this.lucas((n / 2)) * this.fib((n - (n / 2))))) / 2);
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
 * Takes language-independent specification of Fibonacci with Lucas and generates Java code
 */
class FibonacciWithLucasMainJava {
  val generator = CodeGenerator(CodeGenerator.defaultConfig.copy(boxLevel = PartiallyBoxed, targetPackage = new PackageDeclaration(ObjectOriented.fromComponents("fibonacci"))))

  val fibonacciApproach = FibonacciIndependentWithLucasProvider.imperative[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm, generator.imperativeInMethod, generator.intsInMethod, generator.assertionsInMethod, generator.equalityInMethod)

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

object FibonacciWithLucasJavaDirectToDiskMain extends IOApp {
  val targetDirectory = Paths.get("target", "fib", "java")
  print(targetDirectory)
  def run(args: List[String]): IO[ExitCode] = {
    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new FibonacciWithLucasMainJava() }
      _ <- IO { println("[OK]") }
      result <- main.runDirectToDisc(targetDirectory)
    } yield result
  }
}
