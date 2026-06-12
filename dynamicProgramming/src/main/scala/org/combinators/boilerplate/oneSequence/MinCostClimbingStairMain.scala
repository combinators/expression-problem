package org.combinators.boilerplate.oneSequence

import org.combinators.dp.enhanced.{EnhancedDPMainJava, EnhancedDPMainScala}
import org.combinators.dp.original.{BottomUp, TopDown}
import org.combinators.dp.TestExample
import org.combinators.models.*
import org.combinators.models.enhancedModels.oneSequence.MinCostClimbingStair

/**
 * All that is needed here is the set of test cases that you need.
 */
trait MinCostClimbingStairApp {

  val tests = Seq(
    new TestExample("ts1", LiteralArray(Array(1,100,1,1,1,100,1,1,100,1)), LiteralInt(6), new UnitExpression),
    new TestExample("ts2", LiteralArray(Array(10,15,20)), LiteralInt(15), new UnitExpression),
  )

  val model: EnhancedModel = new MinCostClimbingStair().model
}

// Need these two classes to extend appropriate *MainJava or *MainScala
class MinCostClimbingStairMainJava extends EnhancedDPMainJava with MinCostClimbingStairApp {
  override def constructApp(): EnhancedDPMainJava = new MinCostClimbingStairMainJava()
}
class MinCostClimbingStairMainScala extends EnhancedDPMainScala with MinCostClimbingStairApp {
  override def constructApp(): EnhancedDPMainScala = new MinCostClimbingStairMainScala()
}

// need objects to be able to execute as IOApp
object MinCostClimbingStairScalaToDiskMain extends EnhancedDPMainScala with MinCostClimbingStairApp {
  override def constructApp(): EnhancedDPMainScala = new MinCostClimbingStairMainScala()
}
object MinCostClimbingStairJavaToDiskMain extends EnhancedDPMainJava with MinCostClimbingStairApp {
  override def constructApp(): EnhancedDPMainJava = new MinCostClimbingStairMainJava()
}
