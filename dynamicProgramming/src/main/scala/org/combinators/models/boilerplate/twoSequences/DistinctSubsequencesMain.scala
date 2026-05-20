package org.combinators.models.boilerplate.twoSequences

import org.combinators.dp.{BottomUp, TestExample, TopDown}
import org.combinators.dp.enhanced.{EnhancedDPMainJava, EnhancedDPMainScala}
import org.combinators.models.*
import org.combinators.models.enhancedModels.twoSequences.DistinctSubsequences

trait DistinctSubsequencesApp {
  val tests = Seq(
    new TestExample("ds1", new LiteralStringPair("rabbbit", "rabbit"), new LiteralInt(3), new UnitExpression),
    new TestExample("ds2", new LiteralStringPair("babgbag", "bag"),    new LiteralInt(5), new UnitExpression),
  )

  val model: EnhancedModel = new DistinctSubsequences().model
}

// Need these two classes to extend appropriate *MainJava or *MainScala
class DistinctSubsequencesMainJava extends EnhancedDPMainJava with DistinctSubsequencesApp {
  override def constructApp(): EnhancedDPMainJava =  new DistinctSubsequencesMainJava()
}
class DistinctSubsequencesMainScala extends EnhancedDPMainScala with DistinctSubsequencesApp {
  override def constructApp(): EnhancedDPMainScala = new DistinctSubsequencesMainScala()
}

// need objects to be able to execute as IOApp
object DistinctSubsequencesScalaToDiskMain extends EnhancedDPMainScala with DistinctSubsequencesApp {
  override def constructApp(): EnhancedDPMainScala = new DistinctSubsequencesMainScala()
}
object DistinctSubsequencesJavaToDiskMain extends EnhancedDPMainJava with DistinctSubsequencesApp {
  override def constructApp(): EnhancedDPMainJava = new DistinctSubsequencesMainJava()
}
