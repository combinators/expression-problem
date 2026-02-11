package org.combinators.integer.perfectsquare

/**
 * sbt "dp/runMain org.combinators.dp.DPJavaDirectToDiskMain"
 *
 * Creates output files in target/dp
 */

import cats.effect.{ExitCode, IO, IOApp}
import com.github.javaparser.ast.PackageDeclaration
import org.apache.commons.io.FileUtils
import org.combinators.dp.enhanced.EnhancedDPObjectOrientedProvider
import org.combinators.dp.{BottomUp, GenerationOption, TestExample, TopDown}
import org.combinators.ep.generator.FileWithPathPersistable._
import org.combinators.ep.generator.{FileWithPath, FileWithPathPersistable}
import org.combinators.ep.language.java.paradigm.ObjectOriented
import org.combinators.ep.language.java.{CodeGenerator, JavaNameProvider, PartiallyBoxed, Syntax}
import org.combinators.model._

import java.nio.file.{Path, Paths}

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 */
class PerfectSquareMainJava {
  val generator = CodeGenerator(CodeGenerator.defaultConfig.copy(boxLevel = PartiallyBoxed, targetPackage = new PackageDeclaration(ObjectOriented.fromComponents("dp"))))

  val dpApproach = EnhancedDPObjectOrientedProvider[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.imperativeInMethod, generator.doublesInMethod, generator.realDoublesInMethod, generator.consoleInMethod, generator.arraysInMethod, generator.assertionsInMethod, generator.stringsInMethod, generator.equalityInMethod, generator.ooParadigm, generator.parametricPolymorphism, generator.booleansInMethod)(generator.generics)

  val persistable = FileWithPathPersistable[FileWithPath]

  val tests = Seq(
    new TestExample("ps1", new LiteralInt(13), new LiteralInt(2), new UnitExpression), // 9 + 4
    new TestExample("ps2", new LiteralInt(14), new LiteralInt(3), new UnitExpression), // 9 + 4 + 1
    new TestExample("ps3", new LiteralInt(15), new LiteralInt(4), new UnitExpression), // 9 + 4 + 1 + 1
    new TestExample("ps4", new LiteralInt(16), new LiteralInt(1), new UnitExpression), // 16
  )

  def filesToGenerate(model: EnhancedModel, option: GenerationOption): Seq[FileWithPath] = {
    println(s"Generating ${model.problem}...")
    generator.paradigm.runGenerator {
      for {
        _ <- generator.doublesInMethod.enable()
        _ <- generator.realDoublesInMethod.enable()
        _ <- generator.intsInMethod.enable()
        _ <- generator.stringsInMethod.enable()
        _ <- generator.listsInMethod.enable() // should be array, but this still needs to be added as an FFI
        _ <- generator.consoleInMethod.enable()
        _ <- generator.arraysInMethod.enable()
        _ <- generator.equalityInMethod.enable()
        _ <- generator.assertionsInMethod.enable()
        _ <- generator.booleansInMethod.enable()

        // HERE you can finally specify the method to use for testing and the test cases
        _ <- dpApproach.implement(model, tests, option)
      } yield ()
    }
  }

  def directToDiskTransaction(targetDirectory: Path, model: EnhancedModel, option: GenerationOption): IO[Unit] = {

    IO {
      print("Computing Files...")
      val computed = filesToGenerate(model, option)
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

  def runDirectToDisc(targetDirectory: Path, model: EnhancedModel, option: GenerationOption): IO[ExitCode] = {
    for {
      _ <- directToDiskTransaction(targetDirectory, model, option)
    } yield ExitCode.Success
  }
}

object PerfectSquareMainDirectToDiskMain extends IOApp {
  val targetDirectory:Path = Paths.get("target", "dp")

  def model: EnhancedModel = {
    // Needed for conditions and fib(n-1) and fib(n-2)
    val zero: LiteralInt = new LiteralInt(0)
    val one: LiteralInt = new LiteralInt(1)

    // COULD be inferred from the ArgExpression list, but this lets us name variable to use in iterator
    val i: HelperExpression = new HelperExpression("i")    // only one argument, i
    val k: HelperExpression = new HelperExpression("k")

    /* Perfect Square. */
    val n = new ArgExpression(0, "n", new IntegerType(), "i")
    val bound_ps = List(n)

    val symTable_ps = Map("i" -> n)
    val sol_ps = new SubproblemInvocation(symTable_ps)

    val ps_subprobExpr = new AdditionExpression(one, new SubproblemExpression(Seq(new SubtractionExpression(i, new MultiplicationExpression(k, k)))))
    val def_ps = new MinRangeDefinition(Seq(i), k, one, new LessThanExpression(new MultiplicationExpression(k, k), new AdditionExpression(i, one)), ps_subprobExpr, new AdditionExpression(k, one))
    val ps_inner_definition = new IfThenElseDefinition(new EqualExpression(i, one), new ExpressionStatement(one), def_ps)
    val ps_definition = new IfThenElseDefinition(new EqualExpression(i, zero), new ExpressionStatement(zero), ps_inner_definition)

    val PerfectSquare = new EnhancedModel("PerfectSquare",
      bound_ps,
      subproblemType = new LiteralInt(0),    // helper method is an int
      solutionType =  new LiteralString(""), // solution is a string
      sol_ps,
      ps_definition
    )

    PerfectSquare
  }

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
      topDown
    }

    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new PerfectSquareMainJava() }
      _ <- IO { println("[OK]") }

      result <- main.runDirectToDisc(targetDirectory, PerfectSquareMainDirectToDiskMain.model, choice)
    } yield result
  }
}
