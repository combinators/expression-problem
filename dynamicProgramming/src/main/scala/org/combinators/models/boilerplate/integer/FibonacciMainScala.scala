package org.combinators.models.boilerplate.integer

/**
 * sbt "dp/runMain org.combinators.dp.DPJavaDirectToDiskMain"
 *
 * Creates output files in target/dp
 */
import cats.effect.{ExitCode, IO, IOApp}
import org.combinators.dp.enhanced.EnhancedDPMainScala
import org.combinators.dp.{BottomUp, TestExample, TopDown}
import org.combinators.models._
import org.combinators.models.enhancedModels.integer.Fibonacci

import java.nio.file.{Path, Paths}

/**
 * Uses enhanced DP Provide
 */
class FibonacciEnhancedMainScala extends EnhancedDPMainScala {
  override def tests = Seq(
    new TestExample("fib0", new LiteralInt(0), new LiteralInt(0), new UnitExpression), // for now, leave solution as None
    new TestExample("fib1", new LiteralInt(1), new LiteralInt(1), new UnitExpression),
    new TestExample("fib2", new LiteralInt(2), new LiteralInt(1), new UnitExpression),
    new TestExample("fib7", new LiteralInt(7), new LiteralInt(13), new UnitExpression),
    new TestExample("fib20", new LiteralInt(20), new LiteralInt(6765), new UnitExpression),
    new TestExample("fib40", new LiteralInt(40), new LiteralInt(102334155), new UnitExpression)
  )
}

object FibonacciEnhancedMainToDiskMainScala extends IOApp {
  val targetDirectory:Path = Paths.get("target", "dp", "fibonacci")

  val model: EnhancedModel = new Fibonacci().model

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
      main <- IO { new FibonacciEnhancedMainScala() }
      _ <- IO { println("[OK]") }

      result <- main.runDirectToDisc(targetDirectory, model, choice)
    } yield result
  }
}
