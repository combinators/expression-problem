package org.combinators.models.boilerplate.twoSequences

import org.apache.commons.io.FileUtils
import org.combinators.dp.enhanced.{EnhancedDPMainJava, EnhancedDPMainScala, EnhancedMainInterface}
import org.combinators.dp.TestExample
import org.combinators.ep.language.java.paradigm.ObjectOriented
import org.combinators.ep.language.java.{CodeGenerator, JavaNameProvider, Syntax, Unboxed}
import org.combinators.models.*
import org.combinators.cogen.{FileWithPath, FileWithPathPersistable}
import FileWithPathPersistable.*
import org.combinators.dp.original.{BottomUp, GenerationOption, TopDown}
import org.combinators.models.enhancedModels.twoSequences.NeedlemanWunschSequenceAlignment

// needs custom-support code, because the test case has unusual structure.
case class NeedlemanWunschSequenceInputType() extends ArgumentType
class NeedlemanWunschSequenceInput(val string1:String, val string2:String, val matchBonus:Int, val mismatchPenalty:Int, val gapPenalty:Int) extends LiteralExpression {
  def tpe:ArgumentType = NeedlemanWunschSequenceInputType()
}

trait NeedlemanWunschSequenceAlignmentApp {

  def tests = Seq(
    // https://rna.informatik.uni-freiburg.de/Teaching/index.jsp?toolName=Needleman-Wunsch has really nice example
    // from google search via AI so cannot trace,
    new TestExample("nws1", new NeedlemanWunschSequenceInput("abc", "ace", +2, -1, -2), LiteralInt(0), new UnitExpression),
    new TestExample("nws2", new NeedlemanWunschSequenceInput("CTCGCAGC", "CATTCAC", +10, -2, -5), LiteralInt(33), new UnitExpression),
    // https://medium.com/@nandiniumbarkar/needleman-wunsch-algorithm-7bba68b510db
  )

  val model: EnhancedModel = new NeedlemanWunschSequenceAlignment().model
}

// Need these two classes to extend appropriate *MainJava or *MainScala
class NeedlemanWunschSequenceAlignmentMainJava extends EnhancedDPMainJava with NeedlemanWunschSequenceAlignmentApp {
  override def constructApp(): EnhancedDPMainJava = new NeedlemanWunschSequenceAlignmentMainJava()
}
class NeedlemanWunschSequenceAlignmentMainScala extends EnhancedDPMainScala with NeedlemanWunschSequenceAlignmentApp {
  override def constructApp(): EnhancedDPMainScala = new NeedlemanWunschSequenceAlignmentMainScala()
}

// need objects to be able to execute as IOApp
object NeedlemanWunschSequenceAlignmentScalaToDiskMain extends EnhancedDPMainScala with NeedlemanWunschSequenceAlignmentApp {
  override def constructApp(): EnhancedDPMainScala = new NeedlemanWunschSequenceAlignmentMainScala()
}
object NeedlemanWunschSequenceAlignmentJavaToDiskMain extends EnhancedDPMainJava with NeedlemanWunschSequenceAlignmentApp {
  override def constructApp(): EnhancedDPMainJava = new NeedlemanWunschSequenceAlignmentMainJava()
}
