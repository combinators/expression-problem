package org.combinators.boilerplate.grid

import org.combinators.dp.TestExample
import org.combinators.dp.enhanced.{EnhancedDPMainJava, EnhancedDPMainScala}
import org.combinators.models.*
import org.combinators.models.enhancedModels.grid.NumberPathsWithKCoins

trait NumberPathsWithKCoinsApp {
  def tests = Seq(
    new TestExample("npk1", LiteralTuple(LiteralArray(Array(1, 2, 3,
                                               4, 6, 5,
                                               3, 2, 1), Seq(3,3)), LiteralInt(12)), LiteralInt(10), new UnitExpression) , // 12 total count

  )
  val model: EnhancedModel = new NumberPathsWithKCoins().model
}

// Need these two classes to extend appropriate *MainJava or *MainScala
class NumberPathsWithKCoinsMainJava extends EnhancedDPMainJava with NumberPathsWithKCoinsApp {
  override def constructApp(): EnhancedDPMainJava = new NumberPathsWithKCoinsMainJava()
}
class NumberPathsWithKCoinsMainScala extends EnhancedDPMainScala with NumberPathsWithKCoinsApp {
  override def constructApp(): EnhancedDPMainScala = new NumberPathsWithKCoinsMainScala()
}

// need objects to be able to execute as IOApp
object NumberPathsWithKCoinsScalaToDiskMain extends EnhancedDPMainScala with NumberPathsWithKCoinsApp {
  override def constructApp(): EnhancedDPMainScala = new NumberPathsWithKCoinsMainScala()
}
object NumberPathsWithKCoinsJavaToDiskMain extends EnhancedDPMainJava with NumberPathsWithKCoinsApp {
  override def constructApp(): EnhancedDPMainJava = new NumberPathsWithKCoinsMainJava()
}
