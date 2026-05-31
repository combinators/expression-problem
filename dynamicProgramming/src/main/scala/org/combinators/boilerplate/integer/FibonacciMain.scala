package org.combinators.boilerplate.integer

/**
 * sbt "dp/runMain org.combinators.dp.DPJavaDirectToDiskMain"
 *
 * Creates output files in target/dp
 */
import org.combinators.dp.enhanced.{EnhancedDPMainJava, EnhancedDPMainScala}
import org.combinators.dp.original.{BottomUp, TopDown}
import org.combinators.dp.TestExample
import org.combinators.models.*
import org.combinators.models.enhancedModels.integer.Fibonacci

/**
 * Uses enhanced DP Provide
 */
trait FibonacciApp {
  val tests = Seq(
    new TestExample("fib0", LiteralInt(0), LiteralInt(0), new UnitExpression), // for now, leave solution as None
    new TestExample("fib1", LiteralInt(1), LiteralInt(1), new UnitExpression),
    new TestExample("fib2", LiteralInt(2), LiteralInt(1), new UnitExpression),
    new TestExample("fib7", LiteralInt(7), LiteralInt(13), new UnitExpression),
    new TestExample("fib20", LiteralInt(20), LiteralInt(6765), new UnitExpression),
    new TestExample("fib40", LiteralInt(40), LiteralInt(102334155), new UnitExpression)      // Takes some time!
  )

  val model: EnhancedModel = new Fibonacci().model
}

// Need these two classes to extend appropriate *MainJava or *MainScala
class FibonacciMainJava extends EnhancedDPMainJava with FibonacciApp {
  override def constructApp(): EnhancedDPMainJava = new FibonacciMainJava()
}
class FibonacciMainScala extends EnhancedDPMainScala with FibonacciApp {
  override def constructApp(): EnhancedDPMainScala = new FibonacciMainScala()
}

// need objects to be able to execute as IOApp
object FibonacciScalaToDiskMain extends EnhancedDPMainScala with FibonacciApp {
  override def constructApp(): EnhancedDPMainScala = new FibonacciMainScala()
}
object FibonacciJavaToDiskMain extends EnhancedDPMainJava with FibonacciApp {
  override def constructApp(): EnhancedDPMainJava = new FibonacciMainJava()
}
