package org.combinators.boilerplate.grid

import org.combinators.dp.TestExample
import org.combinators.dp.enhanced.{EnhancedDPMainJava, EnhancedDPMainScala}
import org.combinators.models.*
import org.combinators.models.enhancedModels.grid.GridWithObstacles

trait GridWithObstaclesApp {
  def tests = Seq(
    new TestExample("gwo1", LiteralArray(Array(0,0,0,0,
                                               0,0,0,0,
                                               0,0,0,0), Seq(3,4)), LiteralInt(10), new UnitExpression) , // no obstacles
    new TestExample("gwo2", LiteralArray(Array(0,0,0,0,
                                               0,1,1,0,
                                               0,0,0,0), Seq(3,4)), LiteralInt(2), new UnitExpression), // around edges only
    new TestExample("gwo2", LiteralArray(Array(0,0,0,0,0,0,
                                               0,1,1,0,0,0,
                                               0,0,0,1,1,0,
                                               0,1,1,0,0,0,
                                               0,0,0,0,0,0), Seq(5,6)), LiteralInt(4), new UnitExpression), // more complex
  )
  val model: EnhancedModel = new GridWithObstacles().model
}

// Need these two classes to extend appropriate *MainJava or *MainScala
class GridWithObstaclesMainJava extends EnhancedDPMainJava with GridWithObstaclesApp {
  override def constructApp(): EnhancedDPMainJava = new GridWithObstaclesMainJava()
}
class GridWithObstaclesMainScala extends EnhancedDPMainScala with GridWithObstaclesApp {
  override def constructApp(): EnhancedDPMainScala = new GridWithObstaclesMainScala()
}

// need objects to be able to execute as IOApp
object GridWithObstaclesScalaToDiskMain extends EnhancedDPMainScala with GridWithObstaclesApp {
  override def constructApp(): EnhancedDPMainScala = new GridWithObstaclesMainScala()
}
object GridWithObstaclesJavaToDiskMain extends EnhancedDPMainJava with GridWithObstaclesApp {
  override def constructApp(): EnhancedDPMainJava = new GridWithObstaclesMainJava()
}
