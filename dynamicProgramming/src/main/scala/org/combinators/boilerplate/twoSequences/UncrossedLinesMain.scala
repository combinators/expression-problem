package org.combinators.boilerplate.twoSequences

import org.combinators.dp.TestExample
import org.combinators.dp.enhanced.{EnhancedDPMainJava, EnhancedDPMainScala}
import org.combinators.dp.original.{BottomUp, TopDown}
import org.combinators.models.*
import org.combinators.models.enhancedModels.twoSequences.UncrossedLines

trait UncrossedLinesApp {
  val tests = Seq(
    new TestExample("ul1", LiteralTuple(LiteralArray(Array(1, 4, 2)), LiteralArray(Array(1, 2, 4))), LiteralInt(2), new UnitExpression)
  )

  val model: EnhancedModel = new UncrossedLines().model
}

// Need these two classes to extend appropriate *MainJava or *MainScala
class UncrossedLinesMainJava extends EnhancedDPMainJava with UncrossedLinesApp {
  override def constructApp(): EnhancedDPMainJava = new UncrossedLinesMainJava()
}
class UncrossedLinesMainScala extends EnhancedDPMainScala with UncrossedLinesApp {
  override def constructApp(): EnhancedDPMainScala = new UncrossedLinesMainScala()
}

// need objects to be able to execute as IOApp
object UncrossedLinesScalaToDiskMain extends EnhancedDPMainScala with UncrossedLinesApp {
  override def constructApp(): EnhancedDPMainScala = new UncrossedLinesMainScala()
}
object UncrossedLinesJavaToDiskMain extends EnhancedDPMainJava with UncrossedLinesApp {
  override def constructApp(): EnhancedDPMainJava = new UncrossedLinesMainJava()
}
