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

    // Need to find way to get these (i,j) into the EnhancedModel. Apply the mapping that iteration takes place over (r,c) and there is
    // mapping of i = r+c+2 and j = c+1. The inherent problem search is upper triangle matrix of the P(i,j) space, which turns out to
    // be upper left triangular matrix over (r,c)
    val c: HelperExpression = HelperExpression("c", two, SelfExpression("c") <= new ArrayLengthExpression(array), new ArrayLengthExpression(array))
    val r: HelperExpression = HelperExpression("r", one, SelfExpression("r") <= new ArrayLengthExpression(array) - c + one, new ArrayLengthExpression(array))

    // mapping. BOTTOM UP introduce new variables. TOP-DOWN had used variables all along
    val i: HelperExpression = HelperExpression("i", zero, SelfExpression("i") <= new ArrayLengthExpression(array), new ArrayLengthExpression(array))   // MOST of this unnecessary
    val j: HelperExpression = HelperExpression("j", zero, SelfExpression("i") <= new ArrayLengthExpression(array), new ArrayLengthExpression(array))   // MOST of this unnecessary

    val k: HelperExpression = HelperExpression("k", i, SelfExpression("k") < j, new ArrayLengthExpression(array)) // k will always be within this range

    // what the compute() method calls with helper(i = 1, j = nums.length-1) -- THIS IS TOP DOWN but also becomes dp[i][j] for solution in BOTTOM UP
    // not sure why helper(1, N-1) but then dp[1][n] in return. #ANNOYED
    val params = Map(
      "i" -> new LiteralInt(1),
      "j" -> (new ArrayLengthExpression(array) - one)
    )
    val helpers = Map("k" -> k)
    val mappers = Map("i" -> r, "j" -> (r + c - one))         // will control the innermost logic after mapping from the iteration variables
    val sol = SubproblemInvocation(params, Seq("c", "r", "i", "j"), helpers = helpers, mappers = mappers)   // seq(c,r) is for BOTTOM UP only but i,j are included for TOP DOWN

    /*
     * This is a form of decomposition that applies to upper triangle of the P problem space.
     *
     *   P(i,j) = 0, if i == j
     *   P(i,j) = Min (k, P(i,k) + P(k+1,j) + cost of multiplying resulting two matrices)
     *      for (int k = i; k < j; k++)
     */
    val subprobExpr = new SubproblemExpression(Seq(i, k)) + new SubproblemExpression(Seq(k + one, j)) + array(i - one) * array(k) * array(j)

    // Min range definition for k in range from i (inclusive) to j (exclusive) with an advance of k+1
    val defij = MinRangeDefinition("k", i, k < j, subprobExpr, k + one)

    val mcm_definition = IfThenElseDefinition(i == j, ExpressionStatement(zero), defij)

    val MCM = new EnhancedModel("MatrixChainMultiplication",
      bound,
      subproblemType = IntegerType(),  // helper methods and intermediate problems are int
      solutionType   = StringType(),   // how a solution is represented
      sol,
      mcm_definition
    )

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
