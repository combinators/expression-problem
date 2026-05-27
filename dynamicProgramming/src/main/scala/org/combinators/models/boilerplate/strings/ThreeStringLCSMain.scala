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
import org.combinators.models.enhancedModels.strings.ThreeStringLCS

/**
 * All that is needed here is the set of test cases that you need.
 */
trait ThreeStringsLCSApp {

  val tests = Seq(
    new TestExample("ts1", LiteralStringTriple("AGGT12", "12TXAYB", "12XBA"), LiteralInt(2), new UnitExpression),
    new TestExample("ts2", LiteralStringTriple("geeks", "geeksfor", "geeksforgeeks"), LiteralInt(5), new UnitExpression),
    new TestExample("ts3", LiteralStringTriple("abcd1e2", "bc12ea", "bd1ea"), LiteralInt(3), new UnitExpression),
  )

  val model: EnhancedModel = new ThreeStringLCS().model
}

// Need these two classes to extend appropriate *MainJava or *MainScala
class ThreeStringsLCSMainJava extends EnhancedDPMainJava with ThreeStringsLCSApp {
  override def constructApp(): EnhancedDPMainJava = new ThreeStringsLCSMainJava()
}
class ThreeStringsLCSMainScala extends EnhancedDPMainScala with ThreeStringsLCSApp {
  override def constructApp(): EnhancedDPMainScala = new ThreeStringsLCSMainScala()
}

// need objects to be able to execute as IOApp
object ThreeStringsLCSScalaToDiskMain extends EnhancedDPMainScala with ThreeStringsLCSApp {
  override def constructApp(): EnhancedDPMainScala = new ThreeStringsLCSMainScala()
}
object ThreeStringsLCSJavaToDiskMain extends EnhancedDPMainJava with ThreeStringsLCSApp {
  override def constructApp(): EnhancedDPMainJava = new ThreeStringsLCSMainJava()
}
