package org.combinators.oneSequence

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
class MatrixChainMultiplicationMainTopDownJava extends EnhancedDPMainJava {

  override def tests = Seq(
    new TestExample("mm1", new LiteralArray(Array(40, 20, 30, 10, 30)), new LiteralInt(26000), new UnitExpression), //
    new TestExample("mm2", new LiteralArray(Array(2, 1, 3, 4)), new LiteralInt(20), new UnitExpression),            // https://www.geeksforgeeks.org/problems/matrix-chain-multiplication0303/1
    new TestExample("mm3", new LiteralArray(Array(10, 30, 5, 60)), new LiteralInt(4500), new UnitExpression),       // ttps://en.wikipedia.org/wiki/Matrix_chain_multiplication
  )
}

object MatrixChainMultiplicationMainTopDownDirectToDiskMain extends IOApp {
  val targetDirectory:Path = Paths.get("target", "dp")

  def model:EnhancedModel = {
    // Needed for conditions and fib(n-1) and fib(n-2)
    val zero: LiteralInt = new LiteralInt(0)
    val one:  LiteralInt = new LiteralInt(1)
    val two:  LiteralInt = new LiteralInt(2)

    // MatrixChainMultiplication has an array of N+1 integers,representing N 2D Matrices
    val array = new ArgExpression(0, "nums", new IntegerArrayType(), "c")     // not too sure whether 'i' remains a requirement as argument here
    val bound = List(array)

    val i: HelperExpression = HelperExpression("i", zero, SelfExpression("i") <= new ArrayLengthExpression(array), new ArrayLengthExpression(array))   // MOST of this unnecessary
    val j: HelperExpression = HelperExpression("j", zero, SelfExpression("i") <= new ArrayLengthExpression(array), new ArrayLengthExpression(array))   // MOST of this unnecessary

    val k: HelperExpression = HelperExpression("k", i, SelfExpression("k") < j, new ArrayLengthExpression(array)) // k will always be within this range

    val helpers = Map("i" -> i, "j" -> j)
    val sol = SubproblemInvocation(Seq("i", "j"), helpers = helpers)   // seq(c,r) is for BOTTOM UP only but i,j are included for TOP DOWN

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
      mcm_definition,
      mode = UpperTriangle(Seq("i", "j")),

      // answer can be found in dp[1][n]
      answer = new SubproblemExpression(Seq(one, new ArrayLengthExpression(array) - one))
    )

    MCM
  }

  def run(args: List[String]): IO[ExitCode] = {

    // choose one of these to pass in
    val topDown         = TopDown()
    val topDownWithMemo = TopDown(memo = true)

    val choice = topDown

    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new MatrixChainMultiplicationMainTopDownJava() }
      _ <- IO { println("[OK]") }

      result <- main.runDirectToDisc(targetDirectory, model, choice)
    } yield result
  }
}

/**

 Below is what it needs to generate:

 public int computexyz() {
   int c = 2;
   int r = 1;
   int[][] dp = new int[this.A.length+1][this.A.length+1];
   while ((c <= this.A.length - 1)) {
     r = 1;
     while ((r < ((this.A.length - c) + 1))) {
       System.out.println(r + "," + c);
       if (23 > 99) {
         dp[r][((r + c) - 1)] = 0;
       } else {
         int min = 2147483647;
         int k = r;
         int result;
         while ((k < ((r + c) - 1))) {
           System.out.println("  " + k);
             result = ((dp[r][k] + dp[(k + 1)][((r + c) - 1)]) + ((this.A[(r - 1)] * this.A[k]) * this.A[((r + c) - 1)]));
             if ((result < min)) {
               min = result;
             }
           k = (k + 1);
         }
         dp[r][((r + c) - 1)] = min;
       }
       r = (r + 1);
     }
     c = (c + 1);
   }

   for (int i = 0; i <= this.A.length; i++) {
     for (int j = 0; j <= this.A.length; j++) {
       System.out.print(dp[i][j] + "\t");
     }
     System.out.println();
   }
   return dp[1][this.A.length-1];
 }

 */