package org.combinators.boilerplate.twoSequences

import org.combinators.dp.TestExample
import org.combinators.dp.enhanced.{EnhancedDPMainJava, EnhancedDPMainScala}
import org.combinators.dp.original.{BottomUp, TopDown}
import org.combinators.models.*
import org.combinators.models.enhancedModels.twoSequences.LongestCommonSubsequence

trait LongestCommonSubsequenceApp {
  val tests = Seq(
    new TestExample("lcs1", LiteralTuple(LiteralString("abc"), LiteralString("ace")), LiteralInt(2), new UnitExpression),
  )

  val model: EnhancedModel = new LongestCommonSubsequence().model
}

// Need these two classes to extend appropriate *MainJava or *MainScala
class LongestCommonSubsequenceMainJava extends EnhancedDPMainJava with LongestCommonSubsequenceApp {
  override def constructApp(): EnhancedDPMainJava = new LongestCommonSubsequenceMainJava()
}
class LongestCommonSubsequenceMainScala extends EnhancedDPMainScala with LongestCommonSubsequenceApp {
  override def constructApp(): EnhancedDPMainScala = new LongestCommonSubsequenceMainScala()
}

// need objects to be able to execute as IOApp
object LongestCommonSubsequenceScalaToDiskMain extends EnhancedDPMainScala with LongestCommonSubsequenceApp {
  override def constructApp(): EnhancedDPMainScala = new LongestCommonSubsequenceMainScala()
}
object LongestCommonSubsequenceJavaToDiskMain extends EnhancedDPMainJava with LongestCommonSubsequenceApp {
  override def constructApp(): EnhancedDPMainJava = new LongestCommonSubsequenceMainJava()
}
