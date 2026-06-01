package org.combinators.boilerplate.twoSequences

import org.combinators.dp.TestExample
import org.combinators.dp.enhanced.{EnhancedDPMainJava, EnhancedDPMainScala}
import org.combinators.dp.original.{BottomUp, TopDown}
import org.combinators.models.*
import org.combinators.models.enhancedModels.twoSequences.ShortestCommonSupersequence

trait ShortestCommonSupersequenceApp {
  val tests = Seq(
    new TestExample("scs1", LiteralTuple(LiteralString("abac"), LiteralString("cab")), LiteralInt(5), new UnitExpression),
    new TestExample("scs2", LiteralTuple(LiteralString("abc"), LiteralString("ac")), LiteralInt(4), new UnitExpression),
    new TestExample("scs3", LiteralTuple(LiteralString("abc"), LiteralString("abc")), LiteralInt(3), new UnitExpression),
    new TestExample("scs4", LiteralTuple(LiteralString(""), LiteralString("abc")), LiteralInt(3), new UnitExpression),
    new TestExample("scs5", LiteralTuple(LiteralString("abc"), LiteralString("")), LiteralInt(3), new UnitExpression),
  )

  val model: EnhancedModel = new ShortestCommonSupersequence().model
}

// Need these two classes to extend appropriate *MainJava or *MainScala
class ShortestCommonSupersequenceMainJava extends EnhancedDPMainJava with ShortestCommonSupersequenceApp {
  override def constructApp(): EnhancedDPMainJava = new ShortestCommonSupersequenceMainJava()
}
class ShortestCommonSupersequenceMainScala extends EnhancedDPMainScala with ShortestCommonSupersequenceApp {
  override def constructApp(): EnhancedDPMainScala = new ShortestCommonSupersequenceMainScala()
}

// need objects to be able to execute as IOApp
object ShortestCommonSupersequenceScalaToDiskMain extends EnhancedDPMainScala with ShortestCommonSupersequenceApp {
  override def constructApp(): EnhancedDPMainScala = new ShortestCommonSupersequenceMainScala()
}
object ShortestCommonSupersequenceJavaToDiskMain extends EnhancedDPMainJava with ShortestCommonSupersequenceApp {
  override def constructApp(): EnhancedDPMainJava = new ShortestCommonSupersequenceMainJava()
}
