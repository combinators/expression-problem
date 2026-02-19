package org.combinators.modelTests.strings

/**
 * sbt "dp/runMain org.combinators.dp.DPJavaDirectToDiskMain"
 *
 * Creates output files in target/dp
 */
import cats.effect.{ExitCode, IO, IOApp}
import org.combinators.dp.enhanced.EnhancedDPMainJava
import org.combinators.dp.{BottomUp, TestExample, TopDown}
import org.combinators.model._
import org.combinators.model.enhancedModels.InterleaveStrings

import java.nio.file.{Path, Paths}

/**
 * All that is needed here is the set of test cases that you need.
 */
class InterleaveStringsMainJava extends EnhancedDPMainJava {

  override def tests = Seq(
    new TestExample("ils1", new LiteralStringTriple("aabcc", "dbbca", "aadbbcbcac"), new LiteralBoolean(true), new UnitExpression),
    new TestExample("ils1", new LiteralStringTriple("aab", "axy", "aaxaby"), new LiteralBoolean(true), new UnitExpression),
    new TestExample("ils1", new LiteralStringTriple("aab", "axy", "abaaxy"), new LiteralBoolean(false), new UnitExpression),
  )
}

object InterleaveStringsToDiskMain extends IOApp {
  val targetDirectory:Path = Paths.get("target", "dp", "interleaveStrings")

  val model: EnhancedModel = new InterleaveStrings().model

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
      bottomUp
    }

    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new InterleaveStringsMainJava() }
      _ <- IO { println("[OK]") }

      result <- main.runDirectToDisc(targetDirectory, model, choice)
    } yield result
  }
}
