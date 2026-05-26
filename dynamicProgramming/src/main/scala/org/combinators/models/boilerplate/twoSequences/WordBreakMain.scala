package org.combinators.models.boilerplate.twoSequences

import org.combinators.dp.enhanced.{EnhancedDPMainJava, EnhancedDPMainScala, EnhancedMainInterface}
import org.combinators.dp.TestExample
import org.combinators.models.*
import org.combinators.cogen.{FileWithPath, FileWithPathPersistable}
import FileWithPathPersistable.*
import org.combinators.dp.original.{BottomUp, GenerationOption, TopDown}
import org.combinators.ep.language.java.paradigm.ObjectOriented
import org.combinators.ep.language.java.{CodeGenerator, JavaNameProvider, Syntax, Unboxed}
import org.combinators.models.enhancedModels.twoSequences.WordBreak

// needs custom-support code, because the test case has unusual structure.
case class WordBreakInputType() extends ArgumentType
class WordBreakInput(val s:String, val dictionary:Array[String]) extends LiteralExpression {
  def tpe:ArgumentType = WordBreakInputType()
}

trait WordBreakApp {

  val tests = Seq(
    // https://rna.informatik.uni-freiburg.de/Teaching/index.jsp?toolName=Needleman-Wunsch has really nice example
    // from google search via AI so cannot trace,
    new TestExample("wb1", new WordBreakInput("catsanddog", Array("cats","dog","sand","and","cat")), new LiteralBoolean(false), new UnitExpression),
    new TestExample("wb2", new WordBreakInput("leetcode", Array("leet","code")), new LiteralBoolean(true), new UnitExpression)
    // https://medium.com/@nandiniumbarkar/needleman-wunsch-algorithm-7bba68b510db
  )
  
  val model: EnhancedModel = new WordBreak().model
}

// Need these two classes to extend appropriate *MainJava or *MainScala
class WordBreakMainJava extends EnhancedDPMainJava with WordBreakApp {
  override def constructApp(): EnhancedDPMainJava =  new WordBreakMainJava()
}
class WordBreakMainScala extends EnhancedDPMainScala with WordBreakApp {
  override def constructApp(): EnhancedDPMainScala = new WordBreakMainScala()
}

// need objects to be able to execute as IOApp
object WordBreakScalaToDiskMain extends EnhancedDPMainScala with WordBreakApp {
  override def constructApp(): EnhancedDPMainScala = new WordBreakMainScala()
}
object WordBreakJavaToDiskMain extends EnhancedDPMainJava with WordBreakApp {
  override def constructApp(): EnhancedDPMainJava = new WordBreakMainJava()
}
