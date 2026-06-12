package org.combinators.boilerplate.integer

import org.combinators.dp.TestExample
import org.combinators.dp.enhanced.{EnhancedDPMainJava, EnhancedDPMainScala}
import org.combinators.dp.original.{BottomUp, TopDown}
import org.combinators.models.*
import org.combinators.models.enhancedModels.integer.Tribonacci

trait TribonacciApp {
  val tests = Seq(
    new TestExample("trib0", LiteralInt(0), LiteralInt(0), new UnitExpression),
    new TestExample("trib1", LiteralInt(1), LiteralInt(1), new UnitExpression),
    new TestExample("trib2", LiteralInt(2), LiteralInt(1), new UnitExpression),
    new TestExample("trib3", LiteralInt(3), LiteralInt(2), new UnitExpression),
    new TestExample("trib4", LiteralInt(4), LiteralInt(4), new UnitExpression),
    new TestExample("trib5", LiteralInt(5), LiteralInt(7), new UnitExpression),
  )

  val model: EnhancedModel = new Tribonacci().model
}

// Need these two classes to extend appropriate *MainJava or *MainScala
class TribonacciMainJava extends EnhancedDPMainJava with TribonacciApp {
  override def constructApp(): EnhancedDPMainJava = new TribonacciMainJava()
}
class TribonacciMainScala extends EnhancedDPMainScala with TribonacciApp {
  override def constructApp(): EnhancedDPMainScala = new TribonacciMainScala()
}

// need objects to be able to execute as IOApp
object TribonacciScalaToDiskMain extends EnhancedDPMainScala with TribonacciApp {
  override def constructApp(): EnhancedDPMainScala = new TribonacciMainScala()
}
object TribonacciJavaToDiskMain extends EnhancedDPMainJava with TribonacciApp {
  override def constructApp(): EnhancedDPMainJava = new TribonacciMainJava()
}
