package org.combinators.models.boilerplate.twoSequences

import org.combinators.dp.TestExample
import org.combinators.dp.enhanced.{EnhancedDPMainJava, EnhancedDPMainScala}
import org.combinators.dp.original.{BottomUp, TopDown}
import org.combinators.models.*
import org.combinators.models.enhancedModels.twoSequences.WildcardPatternMatching

trait WildcardPatternMatchingApp {
  val tests = Seq(
    new TestExample("wpm1", new LiteralStringPair("a*b*c", "afhuirbfhwnkc"), new LiteralBoolean(true), new UnitExpression),
    new TestExample("wpm2", new LiteralStringPair("a?b?c", "a"), new LiteralBoolean(false), new UnitExpression),
    new TestExample("wpm4", new LiteralStringPair("adceb", "*a*b"), new LiteralBoolean(true), new UnitExpression),
    new TestExample("wpm5", new LiteralStringPair("", "*"), new LiteralBoolean(true), new UnitExpression),
    new TestExample("wpm6", new LiteralStringPair("", ""), new LiteralBoolean(true), new UnitExpression),
  )

  val model: EnhancedModel = new WildcardPatternMatching().model
}

// Need these two classes to extend appropriate *MainJava or *MainScala
class WildcardPatternMatchingMainJava extends EnhancedDPMainJava with WildcardPatternMatchingApp {
  override def constructApp(): EnhancedDPMainJava =  new WildcardPatternMatchingMainJava()
}
class WildcardPatternMatchingMainScala extends EnhancedDPMainScala with WildcardPatternMatchingApp {
  override def constructApp(): EnhancedDPMainScala = new WildcardPatternMatchingMainScala()
}

// need objects to be able to execute as IOApp
object WildcardPatternMatchingScalaToDiskMain extends EnhancedDPMainScala with WildcardPatternMatchingApp {
  override def constructApp(): EnhancedDPMainScala = new WildcardPatternMatchingMainScala()
}
object WildcardPatternMatchingJavaToDiskMain extends EnhancedDPMainJava with WildcardPatternMatchingApp {
  override def constructApp(): EnhancedDPMainJava = new WildcardPatternMatchingMainJava()
}