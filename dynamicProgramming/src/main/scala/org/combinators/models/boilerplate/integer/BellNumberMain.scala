package org.combinators.models.boilerplate.integer

/**
 * sbt "dp/runMain org.combinators.dp.DPJavaDirectToDiskMain"
 *
 * Creates output files in target/dp
 */

import cats.effect.{ExitCode, IO, IOApp}
import org.combinators.dp.enhanced.{EnhancedDPMainJava, EnhancedDPMainScala}
import org.combinators.dp.{BottomUp, TestExample, TopDown}
import org.combinators.models.*
import org.combinators.models.enhancedModels.integer.BellNumber

import java.nio.file.{Path, Paths}

trait BellNumberTests {
  val tests = Seq(
    new TestExample("bn1", new LiteralInt(3), new LiteralInt(5), new UnitExpression), // https://en.wikipedia.org/wiki/Bell_number
    new TestExample("bn2", new LiteralInt(2), new LiteralInt(2), new UnitExpression),
    new TestExample("bn3", new LiteralInt(5), new LiteralInt(52), new UnitExpression),
  )
}

class BellNumberMainJava extends EnhancedDPMainJava with BellNumberTests
class BellNumberMainScala extends EnhancedDPMainScala with BellNumberTests

object BellNumberDirectToDiskMain extends IOApp {
  val targetDirectory:Path = Paths.get("target", "dp", "bellnumber")

  val model: EnhancedModel = new BellNumber().model

  def run(args: List[String]): IO[ExitCode] = {

    // choose one of these to pass in
    val topDown         = TopDown()
    val topDownWithMemo = TopDown(memo = true)
    val bottomUp        = BottomUp()

    val choice = if (args.length == 1) {
      args(0).toLowerCase() match {
        case "topdown" => topDown
        case "topdownwithmemo" => topDownWithMemo
        case "bottomup" => bottomUp
        case _ => ???
      }
    } else {
      bottomUp
    }

    for {
      _ <- IO { print("Initializing Generator...") }
     // main1 <- IO { new BellNumberMainJava() }
      main2 <- IO { new BellNumberMainScala() }
      _ <- IO { println("[OK]") }

      //result <- main1.runDirectToDisc(targetDirectory, BellNumberDirectToDiskMain.model, choice)
      result <- main2.runDirectToDisc(targetDirectory, BellNumberDirectToDiskMain.model, choice)
    } yield result
  }
}
