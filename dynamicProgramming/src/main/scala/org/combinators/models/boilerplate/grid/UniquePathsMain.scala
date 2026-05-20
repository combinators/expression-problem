package org.combinators.models.boilerplate.grid

import cats.effect.{ExitCode, IO, IOApp}
import org.combinators.dp.{BottomUp, TestExample, TopDown}
import org.combinators.dp.enhanced.{EnhancedDPMainJava, EnhancedDPMainScala}
import org.combinators.models.*
import org.combinators.models.enhancedModels.grid.UniquePaths

import java.nio.file.{Path, Paths}

trait UniquePathsApp {
  val tests = Seq(
    new TestExample("up1", new LiteralPair(3, 7), new LiteralInt(28), new UnitExpression),
    new TestExample("up2", new LiteralPair(3, 2), new LiteralInt(3), new UnitExpression),
  )

  val model: EnhancedModel = new UniquePaths().model
}

// Need these two classes to extend appropriate *MainJava or *MainScala
class UniquePathsMainJava extends EnhancedDPMainJava with UniquePathsApp {
  override def constructApp(): EnhancedDPMainJava =  new UniquePathsMainJava()
}
class UniquePathsMainScala extends EnhancedDPMainScala with UniquePathsApp {
  override def constructApp(): EnhancedDPMainScala = new UniquePathsMainScala()
}

// need objects to be able to execute as IOApp
object UniquePathsScalaToDiskMain extends EnhancedDPMainScala with UniquePathsApp {
  override def constructApp(): EnhancedDPMainScala = new UniquePathsMainScala()
}
object UniquePathsJavaToDiskMain extends EnhancedDPMainJava with UniquePathsApp {
  override def constructApp(): EnhancedDPMainJava = new UniquePathsMainJava()
}
