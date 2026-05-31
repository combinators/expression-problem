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
import org.combinators.models.enhancedModels.oneSequence.LongestValidParentheses

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 */
trait LongestValidParenthesesApp {

  val tests = Seq(
    /** https://leetcode.com/problems/longest-valid-parentheses */
    new TestExample("lvp1", LiteralString(")()())"), LiteralInt(4), new UnitExpression),

    /** https://leetcode.com/problems/longest-valid-parentheses/solutions/14133/my-dp-on-solution-without-using-stack-by-nsyp/ */
    new TestExample("lvp2", LiteralString("()(())"), LiteralInt(6), new UnitExpression),
  )
  
  val model: EnhancedModel = new LongestValidParentheses().model
}

// Need these two classes to extend appropriate *MainJava or *MainScala
class LongestValidParenthesesMainJava extends EnhancedDPMainJava with LongestValidParenthesesApp {
  override def constructApp(): EnhancedDPMainJava = new LongestValidParenthesesMainJava()
}
class LongestValidParenthesesMainScala extends EnhancedDPMainScala with LongestValidParenthesesApp {
  override def constructApp(): EnhancedDPMainScala = new LongestValidParenthesesMainScala()
}

// need objects to be able to execute as IOApp
object LongestValidParenthesesScalaToDiskMain extends EnhancedDPMainScala with LongestValidParenthesesApp {
  override def constructApp(): EnhancedDPMainScala = new LongestValidParenthesesMainScala()
}
object LongestValidParenthesesJavaToDiskMain extends EnhancedDPMainJava with LongestValidParenthesesApp {
  override def constructApp(): EnhancedDPMainJava = new LongestValidParenthesesMainJava()
}