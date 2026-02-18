package org.combinators.modelTests.integer.perfectsquare

/**
 * sbt "dp/runMain org.combinators.dp.DPJavaDirectToDiskMain"
 *
 * Creates output files in target/dp
 */

import cats.effect.{ExitCode, IO, IOApp}
import com.github.javaparser.ast.PackageDeclaration
import org.apache.commons.io.FileUtils
import org.combinators.dp.enhanced.{EnhancedDPMainJava, EnhancedDPObjectOrientedProvider}
import org.combinators.dp.{BottomUp, GenerationOption, TestExample, TopDown}
import org.combinators.ep.generator.FileWithPathPersistable._
import org.combinators.ep.generator.{FileWithPath, FileWithPathPersistable}
import org.combinators.ep.language.java.paradigm.ObjectOriented
import org.combinators.ep.language.java.{CodeGenerator, JavaNameProvider, PartiallyBoxed, Syntax}
import org.combinators.model._
import org.combinators.model.enhancedModels.PerfectSquare

import java.nio.file.{Path, Paths}

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 */
class PerfectSquareMainJava extends EnhancedDPMainJava  {

  val tests = Seq(
    new TestExample("ps1", new LiteralInt(13), new LiteralInt(2), new UnitExpression), // 9 + 4
    new TestExample("ps2", new LiteralInt(14), new LiteralInt(3), new UnitExpression), // 9 + 4 + 1
    new TestExample("ps3", new LiteralInt(15), new LiteralInt(4), new UnitExpression), // 9 + 4 + 1 + 1
    new TestExample("ps4", new LiteralInt(16), new LiteralInt(1), new UnitExpression), // 16
  )

}

object PerfectSquareMainDirectToDiskMain extends IOApp {
  val targetDirectory:Path = Paths.get("target", "dp")

  val model: EnhancedModel = new PerfectSquare().model

  def run(args: List[String]): IO[ExitCode] = {

    // choose one of these to pass in
    val topDown         = TopDown()
    val topDownWithMemo = TopDown(memo = true)
    val bottomUp        = BottomUp()

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
