package org.combinators.models.boilerplate.grid

import cats.effect.{ExitCode, IO, IOApp}
import org.combinators.dp.{BottomUp, TestExample, TopDown}
import org.combinators.dp.enhanced.{EnhancedDPMainJava, EnhancedDPMainScala}
import org.combinators.models.*
import org.combinators.models.enhancedModels.grid.MinPathSum

import java.nio.file.{Path, Paths}

trait MinPathSumApp {

  val tests = Seq(
    new TestExample("mps1", new LiteralArray(Array(1,3,1, 1,5,1, 4,2,1), Seq(3,3)), new LiteralInt(7), new UnitExpression),
    new TestExample("mps2", new LiteralArray(Array(1,2,3, 4,5,6), Seq(2,3)), new LiteralInt(12), new UnitExpression),
  )

  val model: EnhancedModel = new MinPathSum().model
}

// Need these two classes to extend appropriate *MainJava or *MainScala
class MinPathSumMainJava extends EnhancedDPMainJava with MinPathSumApp {
  override def constructApp(): EnhancedDPMainJava =  new MinPathSumMainJava()
}
class MinPathSumMainScala extends EnhancedDPMainScala with MinPathSumApp {
  override def constructApp(): EnhancedDPMainScala = new MinPathSumMainScala()
}

// need objects to be able to execute as IOApp
object MinPathSumScalaToDiskMain extends EnhancedDPMainScala with MinPathSumApp {
  override def constructApp(): EnhancedDPMainScala = new MinPathSumMainScala()
}
object MinPathSumJavaToDiskMain extends EnhancedDPMainJava with MinPathSumApp {
  override def constructApp(): EnhancedDPMainJava = new MinPathSumMainJava()
}
