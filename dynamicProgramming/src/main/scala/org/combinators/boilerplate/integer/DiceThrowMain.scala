package org.combinators.boilerplate.integer

/**
 * sbt "dp/runMain org.combinators.dp.DPJavaDirectToDiskMain"
 *
 * Creates output files in target/dp
 */

import org.combinators.dp.enhanced.{EnhancedDPMainJava, EnhancedDPMainScala}
import org.combinators.dp.original.{BottomUp, TopDown}
import org.combinators.dp.TestExample
import org.combinators.models.*
import org.combinators.models.enhancedModels.integer.DiceThrow

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 */
trait DiceThrowApp {
  val tests = Seq(
    new TestExample("dt1", LiteralTuple(LiteralInt(6), LiteralInt(3), LiteralInt(12)), LiteralInt(25), new UnitExpression) //  https://www.geeksforgeeks.org/dsa/dice-throw-dp-30/
  )
  val model: EnhancedModel = new DiceThrow().model
}

// Need these two classes to extend appropriate *MainJava or *MainScala
class DiceThrowMainJava extends EnhancedDPMainJava with DiceThrowApp {
  override def constructApp(): EnhancedDPMainJava = new DiceThrowMainJava()
}
class DiceThrowMainScala extends EnhancedDPMainScala with DiceThrowApp {
  override def constructApp(): EnhancedDPMainScala = new DiceThrowMainScala()
}

// need objects to be able to execute as IOApp
object DiceThrowScalaToDiskMain extends EnhancedDPMainScala with DiceThrowApp {
  override def constructApp(): EnhancedDPMainScala = new DiceThrowMainScala()
}
object DiceThrowJavaToDiskMain extends EnhancedDPMainJava with DiceThrowApp {
  override def constructApp(): EnhancedDPMainJava = new DiceThrowMainJava()
}