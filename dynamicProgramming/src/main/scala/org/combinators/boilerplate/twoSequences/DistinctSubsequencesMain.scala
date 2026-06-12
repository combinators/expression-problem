package org.combinators.boilerplate.twoSequences

import org.combinators.dp.TestExample
import org.combinators.dp.enhanced.{EnhancedDPMainJava, EnhancedDPMainScala}
import org.combinators.dp.original.{BottomUp, TopDown}
import org.combinators.models.*
import org.combinators.models.enhancedModels.twoSequences.DistinctSubsequences

trait DistinctSubsequencesApp {
  val tests = Seq(
    new TestExample("ds1", LiteralTuple(LiteralString("rabbbit"), LiteralString("rabbit")), LiteralInt(3), new UnitExpression),
    new TestExample("ds2", LiteralTuple(LiteralString("babgbag"), LiteralString("bag")),    LiteralInt(5), new UnitExpression),
  )

  val model: EnhancedModel = new DistinctSubsequences().model
}

// Need these two classes to extend appropriate *MainJava or *MainScala
class DistinctSubsequencesMainJava extends EnhancedDPMainJava with DistinctSubsequencesApp {
  override def constructApp(): EnhancedDPMainJava = new DistinctSubsequencesMainJava()
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
