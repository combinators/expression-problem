package org.combinators.modelTests.oneSequence

/**
 * sbt "dp/runMain org.combinators.dp.DPJavaDirectToDiskMain"
 *
 * Creates output files in target/dp
 */
import cats.effect.{ExitCode, IO, IOApp}
import org.combinators.dp.enhanced.EnhancedDPMainJava
import org.combinators.dp.{BottomUp, TestExample, TopDown}
import org.combinators.model._
import org.combinators.model.enhancedModels.MatrixChainMultiplicationBU

import java.nio.file.{Path, Paths}

/**
 * All that is needed here is the set of test cases that you need.
 */
class MatrixChainMultiplicationMainBottomUpJava extends EnhancedDPMainJava {

  override def tests = Seq(
    new TestExample("mm1", new LiteralArray(Array(40, 20, 30, 10, 30)), new LiteralInt(26000), new UnitExpression), //
    new TestExample("mm2", new LiteralArray(Array(2, 1, 3, 4)), new LiteralInt(20), new UnitExpression),            // https://www.geeksforgeeks.org/problems/matrix-chain-multiplication0303/1
    new TestExample("mm3", new LiteralArray(Array(10, 30, 5, 60)), new LiteralInt(4500), new UnitExpression),       // ttps://en.wikipedia.org/wiki/Matrix_chain_multiplication
  )
}

object MatrixChainMultiplicationMainBottomUpDirectToDiskMain extends IOApp {
  val targetDirectory:Path = Paths.get("target", "dp", "matrixChainMultiplicationBottomUp")

  val model: EnhancedModel =  new MatrixChainMultiplicationBU().model

  def run(args: List[String]): IO[ExitCode] = {

    // choose one of these to pass in
    val bottomUp        = BottomUp()

    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new MatrixChainMultiplicationMainBottomUpJava() }
      _ <- IO { println("[OK]") }

      result <- main.runDirectToDisc(targetDirectory, model, bottomUp)
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