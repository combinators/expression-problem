package org.combinators.boilerplate.oneSequence

/**
 * sbt "dp/runMain org.combinators.dp.DPJavaDirectToDiskMain"
 *
 * Creates output files in target/dp
 */
import org.combinators.dp.enhanced.{EnhancedDPMainJava, EnhancedDPMainScala}
import org.combinators.dp.TestExample
import org.combinators.dp.original.BottomUp
import org.combinators.models.*
import org.combinators.models.enhancedModels.oneSequence.MatrixChainMultiplicationBU

/**
 * All that is needed here is the set of test cases that you need.
 */
trait MatrixChainMultiplicationMainBottomUpApp {

  val tests = Seq(
    new TestExample("mm1", LiteralArray(Array(40, 20, 30, 10, 30)), LiteralInt(26000), new UnitExpression), //
    new TestExample("mm2", LiteralArray(Array(2, 1, 3, 4)), LiteralInt(20), new UnitExpression),            // https://www.geeksforgeeks.org/problems/matrix-chain-multiplication0303/1
    new TestExample("mm3", LiteralArray(Array(10, 30, 5, 60)), LiteralInt(4500), new UnitExpression),       // ttps://en.wikipedia.org/wiki/Matrix_chain_multiplication
  )

  val model: EnhancedModel = new MatrixChainMultiplicationBU().model
}

// Need these two classes to extend appropriate *MainJava or *MainScala
class MatrixChainMultiplicationBottomUpMainJava extends EnhancedDPMainJava with MatrixChainMultiplicationMainBottomUpApp {
  override def constructApp(): EnhancedDPMainJava = new MatrixChainMultiplicationBottomUpMainJava()
}
class MatrixChainMultiplicationBottomUpMainScala extends EnhancedDPMainScala with MatrixChainMultiplicationMainBottomUpApp {
  override def constructApp(): EnhancedDPMainScala = new MatrixChainMultiplicationBottomUpMainScala()
}

// need objects to be able to execute as IOApp
object MatrixChainMultiplicationBottomUpScalaToDiskMain extends EnhancedDPMainScala with MatrixChainMultiplicationMainBottomUpApp {
  override def constructApp(): EnhancedDPMainScala = new MatrixChainMultiplicationBottomUpMainScala()
}
object MatrixChainMultiplicationBottomUpJavaToDiskMain extends EnhancedDPMainJava with MatrixChainMultiplicationMainBottomUpApp {
  override def constructApp(): EnhancedDPMainJava = new MatrixChainMultiplicationBottomUpMainJava()
}
