package org.combinators.models.boilerplate.oneSequence

/**
 * sbt "dp/runMain org.combinators.dp.DPJavaDirectToDiskMain"
 *
 * Creates output files in target/dp
 */
import org.combinators.dp.enhanced.{EnhancedDPMainJava, EnhancedDPMainScala}
import org.combinators.dp.{TestExample, TopDown}
import org.combinators.models.*
import org.combinators.models.enhancedModels.oneSequence.MatrixChainMultiplicationTD

/**
 * All that is needed here is the set of test cases that you need.
 */
trait MatrixChainMultiplicationMainTopDownApp {

  val tests = Seq(
    new TestExample("mm1", new LiteralArray(Array(40, 20, 30, 10, 30)), new LiteralInt(26000), new UnitExpression), //
    new TestExample("mm2", new LiteralArray(Array(2, 1, 3, 4)), new LiteralInt(20), new UnitExpression),            // https://www.geeksforgeeks.org/problems/matrix-chain-multiplication0303/1
    new TestExample("mm3", new LiteralArray(Array(10, 30, 5, 60)), new LiteralInt(4500), new UnitExpression),       // ttps://en.wikipedia.org/wiki/Matrix_chain_multiplication
  )
  
  val model: EnhancedModel = new MatrixChainMultiplicationTD().model
}

// Need these two classes to extend appropriate *MainJava or *MainScala
class MatrixChainMultiplicationTopDownMainJava extends EnhancedDPMainJava with MatrixChainMultiplicationMainTopDownApp {
  override def constructApp(): EnhancedDPMainJava =  new MatrixChainMultiplicationTopDownMainJava()
}
class MatrixChainMultiplicationTopDownMainScala extends EnhancedDPMainScala with MatrixChainMultiplicationMainTopDownApp {
  override def constructApp(): EnhancedDPMainScala = new MatrixChainMultiplicationTopDownMainScala()
}

// need objects to be able to execute as IOApp
object MatrixChainMultiplicationTopDownScalaToDiskMain extends EnhancedDPMainScala with MatrixChainMultiplicationMainTopDownApp {
  override def constructApp(): EnhancedDPMainScala = new MatrixChainMultiplicationTopDownMainScala()
}
object MatrixChainMultiplicationTopDownJavaToDiskMain extends EnhancedDPMainJava with MatrixChainMultiplicationMainTopDownApp {
  override def constructApp(): EnhancedDPMainJava = new MatrixChainMultiplicationTopDownMainJava()
}
