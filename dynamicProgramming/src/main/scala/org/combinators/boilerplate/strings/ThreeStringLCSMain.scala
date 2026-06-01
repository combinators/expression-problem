package org.combinators.boilerplate.strings

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
    new TestExample("ts1", LiteralTuple(LiteralString("AGGT12"), LiteralString("12TXAYB"), LiteralString("12XBA")), LiteralInt(2), new UnitExpression),
    new TestExample("ts2", LiteralTuple(LiteralString("geeks"), LiteralString("geeksfor"), LiteralString("geeksforgeeks")), LiteralInt(5), new UnitExpression),
    new TestExample("ts3", LiteralTuple(LiteralString("abcd1e2"), LiteralString("bc12ea"), LiteralString("bd1ea")), LiteralInt(3), new UnitExpression),
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
