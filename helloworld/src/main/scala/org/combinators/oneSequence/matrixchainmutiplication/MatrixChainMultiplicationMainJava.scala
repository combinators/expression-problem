package org.combinators.oneSequence.matrixchainmutiplication

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
 * All that is needed here is the set of test cases that you need.
 */
class MatrixChainMultiplicationMainJava extends EnhancedDPMainJava {

  override def tests = Seq(
    new TestExample("mm1", new LiteralArray(Array(40, 20, 30, 10, 30)), new LiteralInt(26000), new UnitExpression), //
    new TestExample("mm2", new LiteralArray(Array(2, 1, 3, 4)), new LiteralInt(20), new UnitExpression),            // https://www.geeksforgeeks.org/problems/matrix-chain-multiplication0303/1
    new TestExample("mm3", new LiteralArray(Array(10, 30, 5, 60)), new LiteralInt(4500), new UnitExpression),       // ttps://en.wikipedia.org/wiki/Matrix_chain_multiplication
  )
}

object MatrixChainMultiplicationMainDirectToDiskMain extends IOApp {
  val targetDirectory:Path = Paths.get("target", "dp")

  def model:EnhancedModel = {
    // Needed for conditions and fib(n-1) and fib(n-2)
    val zero: LiteralInt = new LiteralInt(0)
    val one: LiteralInt = new LiteralInt(1)

    // MatrixChainMultiplication has an array of N+1 integers,representing N 2D Matrices
    val array = new ArgExpression(0, "nums", new IntegerArrayType(), "i")
    val bound = List(array)

    // COULD be inferred from the ArgExpression list, but this lets us name variable to use in iterator
    val i: HelperExpression = new HelperExpression("i")    // only one argument, i
    val j: HelperExpression = new HelperExpression("j")    // also over nums
    val k: HelperExpression = new HelperExpression("k")

    // what the compute() method calls with helper(1, nums.length-1)
    val symTable = Map("i" -> new LiteralInt(1), "j" -> new SubtractionExpression(new ArrayLengthExpression(array), one))
    val sol = new SubproblemInvocation(symTable)

    /*
     *   P(i,j) = 0, if i == j
     *   P(i,j) = Min (k, P(i,k) + P(k+1,j) + cost of multiplying resulting two matrices)
     *      for (int k = i; k < j; k++)
     */

    val subprobExpr = new AdditionExpression(
      new SubproblemExpression(Seq(i,k)),
      new AdditionExpression(
        new SubproblemExpression(Seq(k + one, j)),
        array(i - one) * array(k) * array(j)
      )
    )

    // Min range definition for k in range from i (inclusive) to j (exclusive) with an advance of k+1
    val defij = new MinRangeDefinition(Seq(i, j), k, i, new LessThanExpression(k, j), subprobExpr, new AdditionExpression(k, one))

    val mcm_definition = new IfThenElseDefinition(new EqualExpression(i, j), new ExpressionStatement(zero), defij)

    val MCM = new EnhancedModel("MatrixChainMultiplication",
      bound,
      subproblemType = new LiteralInt(0),    // helper methods and intermediate problems are int
      solutionType = new LiteralString(""),  // how a solution is represented
      sol,
      mcm_definition)

    MCM
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
      main <- IO { new MatrixChainMultiplicationMainJava() }
      _ <- IO { println("[OK]") }

      result <- main.runDirectToDisc(targetDirectory, model, choice)
    } yield result
  }
}
