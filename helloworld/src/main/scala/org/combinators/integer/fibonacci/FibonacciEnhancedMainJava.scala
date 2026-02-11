package org.combinators.integer.fibonacci

/**
 * sbt "dp/runMain org.combinators.dp.DPJavaDirectToDiskMain"
 *
 * Creates output files in target/dp
 */

import cats.effect.{ExitCode, IO, IOApp}
import org.combinators.dp.enhanced.EnhancedDPMainJava
import org.combinators.dp.{BottomUp, TestExample, TopDown}
import org.combinators.model._

import java.nio.file.{Path, Paths}

/**
 * Uses enhanced DP Provide
 */
class FibonacciEnhancedMainJava extends EnhancedDPMainJava {
  override def tests = Seq(
    new TestExample("fib0", new LiteralInt(0), new LiteralInt(0), new UnitExpression), // for now, leave solution as None
    new TestExample("fib1", new LiteralInt(1), new LiteralInt(1), new UnitExpression),
    new TestExample("fib2", new LiteralInt(2), new LiteralInt(1), new UnitExpression),
    new TestExample("fib7", new LiteralInt(7), new LiteralInt(13), new UnitExpression),
    new TestExample("fib20", new LiteralInt(20), new LiteralInt(6765), new UnitExpression),
    new TestExample("fib40", new LiteralInt(40), new LiteralInt(102334155), new UnitExpression)
  )
}

object FibonacciEnhancedMainDirectToDiskMain extends IOApp {
  val targetDirectory:Path = Paths.get("target", "dp")

  def model:EnhancedModel = {
    // Needed for conditions and fib(n-1) and fib(n-2)
    val zero: LiteralInt = new LiteralInt(0)
    val one: LiteralInt = new LiteralInt(1)
    val two: LiteralInt = new LiteralInt(2)

    // MatrixChainMultiplication has an array of N+1 integers,representing N 2D Matrices
    val n = new ArgExpression(0, "n", new IntegerType(), "i")
    val bound = List(n)

    // COULD be inferred from the ArgExpression list, but this lets us name variable to use in iterator
    val i: HelperExpression = new HelperExpression("i") // only one argument, i

    // what the compute() method calls with helper(1, nums.length-1)
    val symTable = Map("i" -> n)
    val sol = new SubproblemInvocation(symTable)

    val oneCase = new IfThenElseDefinition(i == one, new ExpressionStatement(one),
      new ExpressionDefinition(new SubproblemExpression(Seq(i - one)) + new SubproblemExpression(Seq(i - two))))

    val zeroCase = new IfThenElseDefinition(i == zero, new ExpressionStatement(zero), oneCase)

    val Fib = new EnhancedModel("Fibonacci",
      bound,
      subproblemType = new LiteralInt(0),    // helper methods and intermediate problems are int
      solutionType = new LiteralString(""),  // how a solution is represented (not yet effective)
      sol,
      zeroCase)

    Fib
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
      topDownWithMemo
    }

    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new FibonacciEnhancedMainJava() }
      _ <- IO { println("[OK]") }

      result <- main.runDirectToDisc(targetDirectory, model, choice)
    } yield result
  }
}
