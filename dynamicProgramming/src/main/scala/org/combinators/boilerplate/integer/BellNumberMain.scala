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
import org.combinators.models.enhancedModels.integer.BellNumber

trait BellNumberApp {
  val tests = Seq(
    new TestExample("bn1", LiteralInt(3), LiteralInt(5), new UnitExpression), // https://en.wikipedia.org/wiki/Bell_number
    new TestExample("bn2", LiteralInt(2), LiteralInt(2), new UnitExpression),
    new TestExample("bn3", LiteralInt(5), LiteralInt(52), new UnitExpression),
  )

  val model: EnhancedModel = new BellNumber().model
}

// Need these two classes to extend appropriate *MainJava or *MainScala
class BellNumberMainJava extends EnhancedDPMainJava with BellNumberApp {
  override def constructApp(): EnhancedDPMainJava = new BellNumberMainJava()
}
class BellNumberMainScala extends EnhancedDPMainScala with BellNumberApp {
  override def constructApp(): EnhancedDPMainScala = new BellNumberMainScala()
}

// need objects to be able to execute as IOApp
object BellNumberScalaToDiskMain extends EnhancedDPMainScala with BellNumberApp {
  override def constructApp(): EnhancedDPMainScala = new BellNumberMainScala()
}
object BellNumberJavaToDiskMain extends EnhancedDPMainJava with BellNumberApp {
  override def constructApp(): EnhancedDPMainJava = new BellNumberMainJava()
}