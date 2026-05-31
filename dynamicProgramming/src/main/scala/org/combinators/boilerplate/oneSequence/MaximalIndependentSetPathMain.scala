package org.combinators.boilerplate.oneSequence

/**
 * sbt "dp/runMain org.combinators.dp.DPJavaDirectToDiskMain"
 *
 * Creates output files in target/dp
 */

import org.combinators.dp.enhanced.{EnhancedDPMainJava, EnhancedDPMainScala}
import org.combinators.dp.original.{BottomUp, TopDown}
import org.combinators.dp.TestExample
import org.combinators.models.*
import org.combinators.models.enhancedModels.oneSequence.MaximalIndependentSetPath

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 */
trait MaximalIndependentSetPathApp {

  val tests = Seq(
    // https://canvas.wpi.edu/courses/79353
    new TestExample("sp1", LiteralArray(Array(12,11,13,15)), LiteralInt(27), new UnitExpression),
    new TestExample("sp2", LiteralArray(Array(2,1000,3,1)), LiteralInt(1001), new UnitExpression),
  )
  val model: EnhancedModel = new MaximalIndependentSetPath().model
}

// Need these two classes to extend appropriate *MainJava or *MainScala
class MaximalIndependentSetPathMainJava extends EnhancedDPMainJava with MaximalIndependentSetPathApp {
  override def constructApp(): EnhancedDPMainJava = new MaximalIndependentSetPathMainJava()
}
class MaximalIndependentSetPathMainScala extends EnhancedDPMainScala with MaximalIndependentSetPathApp {
  override def constructApp(): EnhancedDPMainScala = new MaximalIndependentSetPathMainScala()
}

// need objects to be able to execute as IOApp
object MaximalIndependentSetPathScalaToDiskMain extends EnhancedDPMainScala with MaximalIndependentSetPathApp {
  override def constructApp(): EnhancedDPMainScala = new MaximalIndependentSetPathMainScala()
}
object MaximalIndependentSetPathJavaToDiskMain extends EnhancedDPMainJava with MaximalIndependentSetPathApp {
  override def constructApp(): EnhancedDPMainJava = new MaximalIndependentSetPathMainJava()
}