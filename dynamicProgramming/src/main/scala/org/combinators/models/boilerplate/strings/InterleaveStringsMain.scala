package org.combinators.models.boilerplate.strings

/**
 * sbt "dp/runMain org.combinators.dp.DPJavaDirectToDiskMain"
 *
 * Creates output files in target/dp
 */
import org.combinators.dp.enhanced.{EnhancedDPMainJava, EnhancedDPMainScala}
import org.combinators.dp.original.{BottomUp, TopDown}
import org.combinators.dp.TestExample
import org.combinators.models.*
import org.combinators.models.enhancedModels.strings.InterleaveStrings

/**
 * All that is needed here is the set of test cases that you need.
 */
trait InterleaveStringsApp {

  val tests = Seq(
    new TestExample("ils1", LiteralStringTriple("aabcc", "dbbca", "aadbbcbcac"), LiteralBoolean(true), new UnitExpression),
    new TestExample("ils1", LiteralStringTriple("aab", "axy", "aaxaby"), LiteralBoolean(true), new UnitExpression),
    new TestExample("ils1", LiteralStringTriple("aab", "axy", "abaaxy"), LiteralBoolean(false), new UnitExpression),
  )

  val model: EnhancedModel = new InterleaveStrings().model
}

// Need these two classes to extend appropriate *MainJava or *MainScala
class InterleaveStringsMainJava extends EnhancedDPMainJava with InterleaveStringsApp {
  override def constructApp(): EnhancedDPMainJava = new InterleaveStringsMainJava()
}
class InterleaveStringsMainScala extends EnhancedDPMainScala with InterleaveStringsApp {
  override def constructApp(): EnhancedDPMainScala = new InterleaveStringsMainScala()
}

// need objects to be able to execute as IOApp
object InterleaveStringsScalaToDiskMain extends EnhancedDPMainScala with InterleaveStringsApp {
  override def constructApp(): EnhancedDPMainScala = new InterleaveStringsMainScala()
}
object InterleaveStringsJavaToDiskMain extends EnhancedDPMainJava with InterleaveStringsApp {
  override def constructApp(): EnhancedDPMainJava = new InterleaveStringsMainJava()
}
