package org.combinators.modelTests.oneSequence

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
import org.combinators.model.enhancedModels.LongestIncreasingSubsequence

import java.nio.file.{Path, Paths}

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 */
class LongestIncreasingSubsequenceMainJava extends EnhancedDPMainJava  {

  val tests = Seq(
    //new TestExample("dt1", new LiteralTriple(6, 3, 12), new LiteralInt(25), new UnitExpression) //  https://www.geeksforgeeks.org/dsa/dice-throw-dp-30/
    new TestExample("lis1", new LiteralArray(Array(0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15)), new LiteralInt(6), new UnitExpression),
  new TestExample("lis2", new LiteralArray(Array(3, 10, 2, 1, 20)), new LiteralInt(3), new UnitExpression)
  )
}

object LongestIncreasingSubsequenceDirectToDiskMain extends IOApp {
  val targetDirectory:Path = Paths.get("target", "dp", "LIS")

  val model: EnhancedModel = new LongestIncreasingSubsequence().model

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
      topDownWithMemo
    }

    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new LongestIncreasingSubsequenceMainJava() }
      _ <- IO { println("[OK]") }

      result <- main.runDirectToDisc(targetDirectory, LongestIncreasingSubsequenceDirectToDiskMain.model, choice)
    } yield result
  }
}
