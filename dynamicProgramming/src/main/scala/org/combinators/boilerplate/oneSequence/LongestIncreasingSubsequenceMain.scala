package org.combinators.boilerplate.oneSequence

/**
 * sbt "dp/runMain org.combinators.dp.DPJavaDirectToDiskMain"
 *
 * Creates output files in target/dp
 */

import org.combinators.dp.enhanced.{EnhancedDPMainJava, EnhancedDPMainScala}
import org.combinators.dp.original.{BottomUp, TopDown}
import org.combinators.dp.TestExample
import org.combinators.models.*
import org.combinators.models.enhancedModels.oneSequence.LongestIncreasingSubsequence

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 */
trait LongestIncreasingSubsequenceApp {

  val tests = Seq(
    /** https://en.wikipedia.org/wiki/Longest_increasing_subsequence */
    new TestExample("lis1", LiteralArray(Array(0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15)), LiteralInt(6), new UnitExpression),

    /** https://www.geeksforgeeks.org/dsa/longest-increasing-subsequence-dp-3/ */
    new TestExample("lis2", LiteralArray(Array(3, 10, 2, 1, 20)), LiteralInt(3), new UnitExpression)
  )
  
  val model: EnhancedModel = new LongestIncreasingSubsequence().model
}

// Need these two classes to extend appropriate *MainJava or *MainScala
class LongestIncreasingSubsequenceMainJava extends EnhancedDPMainJava with LongestIncreasingSubsequenceApp {
  override def constructApp(): EnhancedDPMainJava = new LongestIncreasingSubsequenceMainJava()
}
class LongestIncreasingSubsequenceMainScala extends EnhancedDPMainScala with LongestIncreasingSubsequenceApp {
  override def constructApp(): EnhancedDPMainScala = new LongestIncreasingSubsequenceMainScala()
}

// need objects to be able to execute as IOApp
object LongestIncreasingSubsequenceScalaToDiskMain extends EnhancedDPMainScala with LongestIncreasingSubsequenceApp {
  override def constructApp(): EnhancedDPMainScala = new LongestIncreasingSubsequenceMainScala()
}
object LongestIncreasingSubsequenceJavaToDiskMain extends EnhancedDPMainJava with LongestIncreasingSubsequenceApp {
  override def constructApp(): EnhancedDPMainJava = new LongestIncreasingSubsequenceMainJava()
}
