package org.combinators.boilerplate.grid

import org.combinators.dp.TestExample
import org.combinators.dp.enhanced.{EnhancedDPMainJava, EnhancedDPMainScala}
import org.combinators.dp.original.{BottomUp, TopDown}
import org.combinators.models._
import org.combinators.models.enhancedModels.grid.CountSquares

trait CountSquaresApp {
  def tests = Seq(
    new TestExample("cs1", LiteralArray(Array(0,1,1,1, 1,1,1,1, 0,1,1,1), Seq(3,4)), LiteralInt(15), new UnitExpression),
    new TestExample("cs2", LiteralArray(Array(0,0,0,0, 0,0,0,0, 0,0,0,0), Seq(3,4)), LiteralInt(0), new UnitExpression)
  )
  val model: EnhancedModel = new CountSquares().model
}

// Need these two classes to extend appropriate *MainJava or *MainScala
class CountSquaresMainJava extends EnhancedDPMainJava with CountSquaresApp {
  override def constructApp(): EnhancedDPMainJava = new CountSquaresMainJava()
}
class CountSquaresMainScala extends EnhancedDPMainScala with CountSquaresApp {
  override def constructApp(): EnhancedDPMainScala = new CountSquaresMainScala()
}

// need objects to be able to execute as IOApp
object CountSquaresScalaToDiskMain extends EnhancedDPMainScala with CountSquaresApp {
  override def constructApp(): EnhancedDPMainScala = new CountSquaresMainScala()
}
object CountSquaresJavaToDiskMain extends EnhancedDPMainJava with CountSquaresApp {
  override def constructApp(): EnhancedDPMainJava = new CountSquaresMainJava()
}
