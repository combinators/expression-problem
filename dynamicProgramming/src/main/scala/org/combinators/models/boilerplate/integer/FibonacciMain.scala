package org.combinators.models.boilerplate.integer

/**
 * sbt "dp/runMain org.combinators.dp.DPJavaDirectToDiskMain"
 *
 * Creates output files in target/dp
 */
import org.combinators.dp.enhanced.{EnhancedDPMainJava, EnhancedDPMainScala}
import org.combinators.dp.{BottomUp, TestExample, TopDown}
import org.combinators.models.*
import org.combinators.models.enhancedModels.integer.Fibonacci

/**
 * Uses enhanced DP Provide
 */
trait FibonacciApp {
  val tests = Seq(
    new TestExample("fib0", new LiteralInt(0), new LiteralInt(0), new UnitExpression), // for now, leave solution as None
    new TestExample("fib1", new LiteralInt(1), new LiteralInt(1), new UnitExpression),
    new TestExample("fib2", new LiteralInt(2), new LiteralInt(1), new UnitExpression),
    new TestExample("fib7", new LiteralInt(7), new LiteralInt(13), new UnitExpression),
    new TestExample("fib20", new LiteralInt(20), new LiteralInt(6765), new UnitExpression),
    new TestExample("fib40", new LiteralInt(40), new LiteralInt(102334155), new UnitExpression)
  )

  val model: EnhancedModel = new Fibonacci().model
}

// Need these two classes to extend appropriate *MainJava or *MainScala
class FibonacciMainJava extends EnhancedDPMainJava with FibonacciApp {
  override def constructApp(): EnhancedDPMainJava =  new FibonacciMainJava()
}
class FibonacciMainScala extends EnhancedDPMainScala with FibonacciApp {
  override def constructApp(): EnhancedDPMainScala = new FibonacciMainScala()
}

// need objects to be able to execute as IOApp
object FibonacciScalaToDiskMain extends EnhancedDPMainScala with FibonacciApp {
  override def constructApp(): EnhancedDPMainScala = new BellNumberMainScala()
}
object FibonacciJavaToDiskMain extends EnhancedDPMainJava with FibonacciApp {
  override def constructApp(): EnhancedDPMainJava = new BellNumberMainJava()
}
