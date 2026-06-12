package org.combinators.boilerplate.grid

import cats.effect.{ExitCode, IO, IOApp}
import org.combinators.dp.TestExample
import org.combinators.dp.enhanced.{EnhancedDPMainJava, EnhancedDPMainScala}
import org.combinators.dp.original.{BottomUp, TopDown}
import org.combinators.models.*
import org.combinators.models.enhancedModels.grid.UniquePaths

import java.nio.file.{Path, Paths}

trait UniquePathsApp {
  val tests = Seq(
    new TestExample("up1", LiteralTuple(LiteralInt(3), LiteralInt(7)), LiteralInt(28), new UnitExpression),
    new TestExample("up2", LiteralTuple(LiteralInt(3), LiteralInt(2)), LiteralInt(3), new UnitExpression),
  )

  val model: EnhancedModel = new UniquePaths().model
}

// Need these two classes to extend appropriate *MainJava or *MainScala
class UniquePathsMainJava extends EnhancedDPMainJava with UniquePathsApp {
  override def constructApp(): EnhancedDPMainJava = new UniquePathsMainJava()
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
