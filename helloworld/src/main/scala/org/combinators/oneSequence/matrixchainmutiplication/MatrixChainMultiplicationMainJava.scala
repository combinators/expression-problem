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
    val one:  LiteralInt = new LiteralInt(1)
    val two:  LiteralInt = new LiteralInt(2)

    // MatrixChainMultiplication has an array of N+1 integers,representing N 2D Matrices
    val array = new ArgExpression(0, "nums", new IntegerArrayType(), "c")     // not too sure whether 'i' remains a requirement as argument here
    val bound = List(array)

    // Need to find way to get these (i,j) into the EnhancedModel
    val c: HelperExpression = HelperExpression("c", two, SelfExpression("c") <= new ArrayLengthExpression(array))            // Chain length
    val i: HelperExpression = HelperExpression("i", one, SelfExpression("i") <= new ArrayLengthExpression(array) - c + one)  // Row starts
    val j: Expression = i + c - one   // last parts aren't truly needed

    val k: HelperExpression = HelperExpression("k", i, SelfExpression("k") < j)

    // what the compute() method calls with helper(1, nums.length-1)
    val params = Map(
      "c" -> (new ArrayLengthExpression(array), c),
      "i" -> (new LiteralInt(1), i))
    val sol = SubproblemInvocation(params, Seq("i", "c"))

    /*
     *   P(i,j) = 0, if i == j
     *   P(i,j) = Min (k, P(i,k) + P(k+1,j) + cost of multiplying resulting two matrices)
     *      for (int k = i; k < j; k++)
     */

    val subprobExpr = new AdditionExpression(
      new SubproblemExpression(Seq(i, k)),
      new SubproblemExpression(Seq(k + one, j)) + array(i - one) * array(k) * array(j)
    )

    // Min range definition for k in range from i (inclusive) to j (exclusive) with an advance of k+1
    val defij = MinRangeDefinition(Seq(c, i), k, i, k < j, subprobExpr, k + one)

    val mcm_definition = IfThenElseDefinition(i == j, ExpressionStatement(zero), defij)

    val MCM = new EnhancedModel("MatrixChainMultiplication",
      bound,
      subproblemType = IntegerType(),  // helper methods and intermediate problems are int
      solutionType   = StringType(),   // how a solution is represented
      sol,
      mcm_definition)

    MCM
  }

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
      main <- IO { new MatrixChainMultiplicationMainJava() }
      _ <- IO { println("[OK]") }

      result <- main.runDirectToDisc(targetDirectory, model, choice)
    } yield result
  }
}
