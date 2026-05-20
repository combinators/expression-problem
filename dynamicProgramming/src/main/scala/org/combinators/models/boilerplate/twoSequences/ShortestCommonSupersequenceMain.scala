package org.combinators.models.boilerplate.twoSequences

import org.combinators.dp.{BottomUp, TestExample, TopDown}
import org.combinators.dp.enhanced.{EnhancedDPMainJava, EnhancedDPMainScala}
import org.combinators.models.*
import org.combinators.models.enhancedModels.twoSequences.ShortestCommonSupersequence

trait ShortestCommonSupersequenceApp {
  val tests = Seq(
    new TestExample("scs1", new LiteralStringPair("abac", "cab"), new LiteralInt(5), new UnitExpression),
    new TestExample("scs2", new LiteralStringPair("abc", "ac"), new LiteralInt(4), new UnitExpression),
    new TestExample("scs3", new LiteralStringPair("abc", "abc"), new LiteralInt(3), new UnitExpression),
    new TestExample("scs4", new LiteralStringPair("", "abc"), new LiteralInt(3), new UnitExpression),
    new TestExample("scs5", new LiteralStringPair("abc", ""), new LiteralInt(3), new UnitExpression),
  )

  val model: EnhancedModel = new ShortestCommonSupersequence().model
}

// Need these two classes to extend appropriate *MainJava or *MainScala
class ShortestCommonSupersequenceMainJava extends EnhancedDPMainJava with ShortestCommonSupersequenceApp {
  override def constructApp(): EnhancedDPMainJava =  new ShortestCommonSupersequenceMainJava()
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
