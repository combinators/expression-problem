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
import org.combinators.models.enhancedModels.integer.PerfectSquares

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 */
trait PerfectSquaresApp {
  val tests = Seq(
    new TestExample("ps1", LiteralInt(13), LiteralInt(2), new UnitExpression), // 9 + 4
    new TestExample("ps2", LiteralInt(14), LiteralInt(3), new UnitExpression), // 9 + 4 + 1
    new TestExample("ps3", LiteralInt(15), LiteralInt(4), new UnitExpression), // 9 + 4 + 1 + 1
    new TestExample("ps4", LiteralInt(16), LiteralInt(1), new UnitExpression), // 16
  )
  val model: EnhancedModel = new PerfectSquares().model
}

// Need these two classes to extend appropriate *MainJava or *MainScala
class PerfectSquaresMainJava extends EnhancedDPMainJava with PerfectSquaresApp {
  override def constructApp(): EnhancedDPMainJava = new PerfectSquaresMainJava()
}
class PerfectSquaresMainScala extends EnhancedDPMainScala with PerfectSquaresApp {
  override def constructApp(): EnhancedDPMainScala = new PerfectSquaresMainScala()
}

// need objects to be able to execute as IOApp
object PerfectSquaresScalaToDiskMain extends EnhancedDPMainScala with PerfectSquaresApp {
  override def constructApp(): EnhancedDPMainScala = new PerfectSquaresMainScala()
}
object PerfectSquaresJavaToDiskMain extends EnhancedDPMainJava with PerfectSquaresApp {
  override def constructApp(): EnhancedDPMainJava = new PerfectSquaresMainJava()
}
