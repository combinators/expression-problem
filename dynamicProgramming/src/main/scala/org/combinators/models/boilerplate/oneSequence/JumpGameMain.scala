package org.combinators.models.boilerplate.oneSequence

import org.combinators.dp.enhanced.{EnhancedDPMainJava, EnhancedDPMainScala}
import org.combinators.dp.{BottomUp, TestExample, TopDown}
import org.combinators.models.*
import org.combinators.models.enhancedModels.oneSequence.JumpGame

/**
 * All that is needed here is the set of test cases that you need.
 */
trait JumpGameApp {

  val tests = Seq(
    new TestExample("ts1", new LiteralArray(Array(1, 100, 1, 1, 1, 100, 1, 1, 100, 1)), new LiteralInt(6), new UnitExpression),
    new TestExample("ts2", new LiteralArray(Array(10, 15, 20)), new LiteralInt(15), new UnitExpression),
  )

  val model: EnhancedModel = new JumpGame().model
}

// Need these two classes to extend appropriate *MainJava or *MainScala
class JumpGameMainJava extends EnhancedDPMainJava with JumpGameApp {
  override def constructApp(): EnhancedDPMainJava =  new JumpGameMainJava()
}
class JumpGameMainScala extends EnhancedDPMainScala with JumpGameApp {
  override def constructApp(): EnhancedDPMainScala = new JumpGameMainScala()
}

// need objects to be able to execute as IOApp
object JumpGameScalaToDiskMain extends EnhancedDPMainScala with JumpGameApp {
  override def constructApp(): EnhancedDPMainScala = new JumpGameMainScala()
}
object JumpGameJavaToDiskMain extends EnhancedDPMainJava with JumpGameApp {
  override def constructApp(): EnhancedDPMainJava = new JumpGameMainJava()
}