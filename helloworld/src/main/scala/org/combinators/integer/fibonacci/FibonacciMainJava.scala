package org.combinators.integer.fibonacci

/**
 * sbt "dp/runMain org.combinators.dp.DPJavaDirectToDiskMain"
 *
 * Creates output files in target/dp
 */

import cats.effect.{ExitCode, IO, IOApp}
import com.github.javaparser.ast.PackageDeclaration
import org.apache.commons.io.FileUtils
import org.combinators.dp.{BottomUp, GenerationOption, TopDown}
import org.combinators.ep.generator.FileWithPathPersistable._
import org.combinators.ep.generator.{FileWithPath, FileWithPathPersistable}
import org.combinators.ep.language.java.paradigm.ObjectOriented
import org.combinators.ep.language.java.{CodeGenerator, JavaNameProvider, PartiallyBoxed, Syntax}
import org.combinators.model._

import java.nio.file.{Path, Paths}

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 */
class FibonacciMainJava {
  val generator = CodeGenerator(CodeGenerator.defaultConfig.copy(boxLevel = PartiallyBoxed, targetPackage = new PackageDeclaration(ObjectOriented.fromComponents("dp"))))

  val dpApproach = FibonacciProvider[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.imperativeInMethod, generator.doublesInMethod, generator.realDoublesInMethod, generator.consoleInMethod, generator.arraysInMethod, generator.assertionsInMethod, generator.stringsInMethod, generator.equalityInMethod, generator.ooParadigm, generator.parametricPolymorphism, generator.booleansInMethod)(generator.generics)

  val persistable = FileWithPathPersistable[FileWithPath]

  def directToDiskTransaction(targetDirectory: Path, model:Model, option:GenerationOption): IO[Unit] = {

    val files =
      () => generator.paradigm.runGenerator {
        for {
          _ <- generator.doublesInMethod.enable()
          _ <- generator.realDoublesInMethod.enable()
          _ <- generator.intsInMethod.enable()
          _ <- generator.stringsInMethod.enable()
          _ <- generator.listsInMethod.enable()     // should be array, but this still needs to be added as an FFI
          _ <- generator.consoleInMethod.enable()
          _ <- generator.arraysInMethod.enable()
          _ <- generator.equalityInMethod.enable()
          _ <- generator.assertionsInMethod.enable()
          _ <- generator.booleansInMethod.enable()

          // HERE you can finally specify the method to use for testing and the test cases
          _ <- dpApproach.implement(model, option)
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

  def runDirectToDisc(targetDirectory: Path, model:Model, option:GenerationOption): IO[ExitCode] = {
    for {
      _ <- directToDiskTransaction(targetDirectory, model, option)
    } yield ExitCode.Success
  }
}

object FibonacciMainDirectToDiskMain extends IOApp {
  val targetDirectory:Path = Paths.get("target", "dp")

  def run(args: List[String]): IO[ExitCode] = {

    // choose one of these to pass in
    val topDown         = new TopDown()
    val topDownWithMemo = new TopDown(memo = true)
    val bottomUp        = new BottomUp()

    val choice = if (args.length == 1) {
        args(0).toLowerCase() match {
          case "topdown" => topDown
          case "topdownwithmemo" => topDownWithMemo
          case "bottomUp" => bottomUp
          case _ => ???
        }
    } else {
      bottomUp
    }

    // Needed for conditions and fib(n-1) and fib(n-2)
    val zero: LiteralInt = new LiteralInt(0)
    val one: LiteralInt = new LiteralInt(1)
    val two: LiteralInt = new LiteralInt(2)

    // Fibonacci has a single integer argument
    val bound = List(new ArgExpression(0, "n", new IntegerType(), "i"))

    // COULD be inferred from the ArgExpression list, but this lets us name variable to use in iterator
    val n: IteratorExpression = new IteratorExpression(0, "i")   // only one argument, i

    val im1 = new SubtractionExpression(n, one)
    val im2 = new SubtractionExpression(n, two)
n
    val Fib = new Model("Fibonacci",
      bound,
      cases = List(
        ( Some(new EqualExpression(n, zero)),  zero ),
        ( Some(new EqualExpression(n, one)),   one ),
        ( None, new AdditionExpression(new SubproblemExpression(Seq(im1)), new SubproblemExpression(Seq(im2))) )
      )
    )

    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new FibonacciMainJava() }
      _ <- IO { println("[OK]") }

      result <- main.runDirectToDisc(targetDirectory, Fib, choice)
    } yield result
  }
}
