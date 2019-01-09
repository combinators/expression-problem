package example.expression.performance

import example.expression.domain._
import example.expression.j._
import example.expression.oo._
import System.nanoTime

import com.github.javaparser.ast.CompilationUnit
import example.expression.algebra.AlgebraGenerator
import example.expression.extensibleVisitor.ExtensibleVisitorGenerator
import example.expression.interpreter.InterpreterGenerator
import example.expression.scalaVisitor.VisitorGenerator
import example.expression.trivially.TriviallyGenerator

case class Score (n:Int,  min:Long,  average:Long,  max:Long)

/** Execute twenty times, and take lowest. */
object Sample {
  val numTrials:Int = 20

  def sample(block: => Long): Score = {

    // sure there is easier way to do this
    var minV = Long.MaxValue
    var totalV = 0L
    var maxV = 0L
    for (i <- 1 to numTrials) {
      System.gc()
      val time = block
      if (time < minV) { minV = time }
      if (time > maxV) { maxV = time }
      totalV += time
    }

    val avgV = totalV / numTrials
    Score(numTrials, minV, avgV, maxV)
  }
}

abstract class OOTest {
  val gen: WithDomain[MathDomain] with OOGenerator with JUnitTestGenerator

  // time the synthesis of the generated code plus test suites
  def generatedCode(t: Long = nanoTime): Long = {
    val results:Seq[CompilationUnit] = gen.generatedCode() ++ gen.generateSuite(Some("oo"))
    nanoTime - t
  }
}

abstract class TriviallyTest {
  val gen: WithDomain[MathDomain] with TriviallyGenerator with JUnitTestGenerator

  // time the synthesis of the generated code plus test suites
  def generatedCode(t: Long = nanoTime): Long = {
    val results:Seq[CompilationUnit] = gen.generatedCode() ++ gen.generateSuite(Some("oo"))
    nanoTime - t
  }
}

abstract class AlgebraTest {
  val gen: WithDomain[MathDomain] with AlgebraGenerator with JUnitTestGenerator

  // time the synthesis of the generated code plus test suites
  def generatedCode(t: Long = nanoTime): Long = {
    val results:Seq[CompilationUnit] = gen.generatedCode() ++ gen.generateSuite(Some("oo"))
    nanoTime - t
  }
}

abstract class InterpreterTest {
  val gen: WithDomain[MathDomain] with InterpreterGenerator with JUnitTestGenerator

  // time the synthesis of the generated code plus test suites
  def generatedCode(t: Long = nanoTime): Long = {
    val results:Seq[CompilationUnit] = gen.generatedCode() ++ gen.generateSuite(Some("oo"))
    nanoTime - t
  }
}

abstract class VisitorTest {
  val gen: WithDomain[MathDomain] with VisitorGenerator with JUnitTestGenerator

  // time the synthesis of the generated code plus test suites
  def generatedCode(t: Long = nanoTime): Long = {
    val results:Seq[CompilationUnit] = gen.generatedCode() ++ gen.generateSuite(Some("oo"))
    nanoTime - t
  }
}

abstract class ExtensibleVisitorTest {
  val gen: WithDomain[MathDomain] with ExtensibleVisitorGenerator with JUnitTestGenerator

  // time the synthesis of the generated code plus test suites
  def generatedCode(t: Long = nanoTime): Long = {
    val results:Seq[CompilationUnit] = gen.generatedCode() ++ gen.generateSuite(Some("oo"))
    nanoTime - t
  }
}

// might be easier way to do this...
object OOEvaluateTest  {

  def name : String = "oo"

  def evaluate() : Map[String,Score] = {

  val e0 = new OOTest {
    override val gen = new WithDomain(MathDomain) with OOGenerator with JUnitTestGenerator with e0
  }
  val e1 = new OOTest {
    override val gen = new WithDomain(MathDomain) with OOGenerator with JUnitTestGenerator with e0 with e1
  }
  val e2 = new OOTest {
    override val gen = new WithDomain(MathDomain) with OOGenerator with JUnitTestGenerator with e0 with e1 with e2
  }
  val e3 = new OOTest {
    override val gen = new WithDomain(MathDomain) with OOGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3
  }
  val e4 = new OOTest {
    override val gen = new WithDomain(MathDomain) with OOGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4
  }
  val e5 = new OOTest {
    override val gen = new WithDomain(MathDomain) with OOGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
  }
  val e6 = new OOTest {
    override val gen = new WithDomain(MathDomain) with OOGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
  }

    // get thing started. Burn this time to ensure we don't get biased by first run.
    e0.generatedCode()
    e1.generatedCode()
    e2.generatedCode()
    e3.generatedCode()
    e4.generatedCode()
    e5.generatedCode()
    e6.generatedCode()

    Map(
      "e0" -> Sample.sample ({e0.generatedCode()}),
      "e1" -> Sample.sample ({e1.generatedCode()}),
      "e2" -> Sample.sample ({e2.generatedCode()}),
      "e3" -> Sample.sample ({e3.generatedCode()}),
      "e4" -> Sample.sample ({e4.generatedCode()}),
      "e5" -> Sample.sample ({e5.generatedCode()}),
      "e6" -> Sample.sample ({e6.generatedCode()})
    )
  }
}

// might be easier way to do this...
object TriviallyEvaluateTest extends App {

  def name : String = "trivially"

  def evaluate() : Map[String,Score] = {

  val e0 = new TriviallyTest {
    override val gen = new WithDomain(MathDomain) with TriviallyGenerator with JUnitTestGenerator with e0
  }
  val e1 = new TriviallyTest {
    override val gen = new WithDomain(MathDomain) with TriviallyGenerator with JUnitTestGenerator with e0 with e1
  }
  val e2 = new TriviallyTest {
    override val gen = new WithDomain(MathDomain) with TriviallyGenerator with JUnitTestGenerator with e0 with e1 with e2
  }
  val e3 = new TriviallyTest {
    override val gen = new WithDomain(MathDomain) with TriviallyGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3
  }
  val e4 = new TriviallyTest {
    override val gen = new WithDomain(MathDomain) with TriviallyGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4
  }
  val e5 = new TriviallyTest {
    override val gen = new WithDomain(MathDomain) with TriviallyGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
  }
  val e6 = new TriviallyTest {
    override val gen = new WithDomain(MathDomain) with TriviallyGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
  }

    // get thing started. Burn this time to ensure we don't get biased by first run.
    e0.generatedCode()
    e1.generatedCode()
    e2.generatedCode()
    e3.generatedCode()
    e4.generatedCode()
    e5.generatedCode()
    e6.generatedCode()

    Map(
      "e0" -> Sample.sample ({e0.generatedCode()}),
      "e1" -> Sample.sample ({e1.generatedCode()}),
      "e2" -> Sample.sample ({e2.generatedCode()}),
      "e3" -> Sample.sample ({e3.generatedCode()}),
      "e4" -> Sample.sample ({e4.generatedCode()}),
      "e5" -> Sample.sample ({e5.generatedCode()}),
      "e6" -> Sample.sample ({e6.generatedCode()})
    )
  }
}


// might be easier way to do this...
object AlgebraEvaluateTest extends App {

  def name : String = "algebra"

  def evaluate() : Map[String,Score] = {

  val e0 = new AlgebraTest {
    override val gen = new WithDomain(MathDomain) with AlgebraGenerator with JUnitTestGenerator with e0
  }
  val e1 = new AlgebraTest {
    override val gen = new WithDomain(MathDomain) with AlgebraGenerator with JUnitTestGenerator with e0 with e1
  }
  val e2 = new AlgebraTest {
    override val gen = new WithDomain(MathDomain) with AlgebraGenerator with JUnitTestGenerator with e0 with e1 with e2
  }
  val e3 = new AlgebraTest {
    override val gen = new WithDomain(MathDomain) with AlgebraGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3
  }
  val e4 = new AlgebraTest {
    override val gen = new WithDomain(MathDomain) with AlgebraGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4
  }
  val e5 = new AlgebraTest {
    override val gen = new WithDomain(MathDomain) with AlgebraGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
  }
  val e6 = new AlgebraTest {
    override val gen = new WithDomain(MathDomain) with AlgebraGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
  }


    // get thing started. Burn this time to ensure we don't get biased by first run.
    e0.generatedCode()
    e1.generatedCode()
    e2.generatedCode()
    e3.generatedCode()
    e4.generatedCode()
    e5.generatedCode()
    e6.generatedCode()

    Map(
      "e0" -> Sample.sample ({e0.generatedCode()}),
      "e1" -> Sample.sample ({e1.generatedCode()}),
      "e2" -> Sample.sample ({e2.generatedCode()}),
      "e3" -> Sample.sample ({e3.generatedCode()}),
      "e4" -> Sample.sample ({e4.generatedCode()}),
      "e5" -> Sample.sample ({e5.generatedCode()}),
      "e6" -> Sample.sample ({e6.generatedCode()})
    )
  }
}


// might be easier way to do this...
object InterpreterEvaluateTest extends App {

  def name : String = "interpreter"


  def evaluate() : Map[String,Score] = {


  val e0 = new InterpreterTest {
    override val gen = new WithDomain(MathDomain) with InterpreterGenerator with JUnitTestGenerator with e0
  }
  val e1 = new InterpreterTest {
    override val gen = new WithDomain(MathDomain) with InterpreterGenerator with JUnitTestGenerator with e0 with e1
  }
  val e2 = new InterpreterTest {
    override val gen = new WithDomain(MathDomain) with InterpreterGenerator with JUnitTestGenerator with e0 with e1 with e2
  }
  val e3 = new InterpreterTest {
    override val gen = new WithDomain(MathDomain) with InterpreterGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3
  }
  val e4 = new InterpreterTest {
    override val gen = new WithDomain(MathDomain) with InterpreterGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4
  }
  val e5 = new InterpreterTest {
    override val gen = new WithDomain(MathDomain) with InterpreterGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
  }
  val e6 = new InterpreterTest {
    override val gen = new WithDomain(MathDomain) with InterpreterGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
  }

    // get thing started. Burn this time to ensure we don't get biased by first run.
    e0.generatedCode()
    e1.generatedCode()
    e2.generatedCode()
    e3.generatedCode()
    e4.generatedCode()
    e5.generatedCode()
    e6.generatedCode()

    Map(
      "e0" -> Sample.sample ({e0.generatedCode()}),
      "e1" -> Sample.sample ({e1.generatedCode()}),
      "e2" -> Sample.sample ({e2.generatedCode()}),
      "e3" -> Sample.sample ({e3.generatedCode()}),
      "e4" -> Sample.sample ({e4.generatedCode()}),
      "e5" -> Sample.sample ({e5.generatedCode()}),
      "e6" -> Sample.sample ({e6.generatedCode()})
    )
  }
}


// might be easier way to do this...
object VisitorEvaluateTest extends App {

  def name : String = "visitor"


  def evaluate() : Map[String,Score] = {

  val e0 = new VisitorTest {
    override val gen = new WithDomain(MathDomain) with VisitorGenerator with JUnitTestGenerator with e0
  }
  val e1 = new VisitorTest {
    override val gen = new WithDomain(MathDomain) with VisitorGenerator with JUnitTestGenerator with e0 with e1
  }
  val e2 = new VisitorTest {
    override val gen = new WithDomain(MathDomain) with VisitorGenerator with JUnitTestGenerator with e0 with e1 with e2
  }
  val e3 = new VisitorTest {
    override val gen = new WithDomain(MathDomain) with VisitorGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3
  }
  val e4 = new VisitorTest {
    override val gen = new WithDomain(MathDomain) with VisitorGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4
  }
  val e5 = new VisitorTest {
    override val gen = new WithDomain(MathDomain) with VisitorGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
  }
  val e6 = new VisitorTest {
    override val gen = new WithDomain(MathDomain) with VisitorGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
  }

    // get thing started. Burn this time to ensure we don't get biased by first run.
    e0.generatedCode()
    e1.generatedCode()
    e2.generatedCode()
    e3.generatedCode()
    e4.generatedCode()
    e5.generatedCode()
    e6.generatedCode()

    Map(
      "e0" -> Sample.sample ({e0.generatedCode()}),
      "e1" -> Sample.sample ({e1.generatedCode()}),
      "e2" -> Sample.sample ({e2.generatedCode()}),
      "e3" -> Sample.sample ({e3.generatedCode()}),
      "e4" -> Sample.sample ({e4.generatedCode()}),
      "e5" -> Sample.sample ({e5.generatedCode()}),
      "e6" -> Sample.sample ({e6.generatedCode()})
    )
  }
}


// might be easier way to do this...
object ExtensibleVisitorEvaluateTest extends App {

  def name : String = "extensible"

  def evaluate() : Map[String,Score] = {

  val e0 = new ExtensibleVisitorTest {
    override val gen = new WithDomain(MathDomain) with ExtensibleVisitorGenerator with JUnitTestGenerator with e0
  }
  val e1 = new ExtensibleVisitorTest {
    override val gen = new WithDomain(MathDomain) with ExtensibleVisitorGenerator with JUnitTestGenerator with e0 with e1
  }
  val e2 = new ExtensibleVisitorTest {
    override val gen = new WithDomain(MathDomain) with ExtensibleVisitorGenerator with JUnitTestGenerator with e0 with e1 with e2
  }
  val e3 = new ExtensibleVisitorTest {
    override val gen = new WithDomain(MathDomain) with ExtensibleVisitorGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3
  }
  val e4 = new ExtensibleVisitorTest {
    override val gen = new WithDomain(MathDomain) with ExtensibleVisitorGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4
  }
  val e5 = new ExtensibleVisitorTest {
    override val gen = new WithDomain(MathDomain) with ExtensibleVisitorGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
  }
  val e6 = new ExtensibleVisitorTest {
    override val gen = new WithDomain(MathDomain) with ExtensibleVisitorGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
  }

    // get thing started. Burn this time to ensure we don't get biased by first run.
    e0.generatedCode()
    e1.generatedCode()
    e2.generatedCode()
    e3.generatedCode()
    e4.generatedCode()
    e5.generatedCode()
    e6.generatedCode()

    Map(
      "e0" -> Sample.sample ({e0.generatedCode()}),
      "e1" -> Sample.sample ({e1.generatedCode()}),
      "e2" -> Sample.sample ({e2.generatedCode()}),
      "e3" -> Sample.sample ({e3.generatedCode()}),
      "e4" -> Sample.sample ({e4.generatedCode()}),
      "e5" -> Sample.sample ({e5.generatedCode()}),
      "e6" -> Sample.sample ({e6.generatedCode()})
    )
  }
}

object RunAll extends App {
  println ("Generating data...")

  case class Result(name:String, scores:Map[String,Score])

  //
  println("Calibrating") // Burn first one
  OOEvaluateTest.evaluate()

  println ("OO")
  val oo      = Result("oo", OOEvaluateTest.evaluate())
  println ("Visitor")
  val visitor = Result("visitor", VisitorEvaluateTest.evaluate())
  println ("Trivially")
  val trivially = Result("trivially", TriviallyEvaluateTest.evaluate())
  println ("Extensible")
  val extensible = Result("extensible", ExtensibleVisitorEvaluateTest.evaluate())
  println ("Interpreter")
  val interpreter = Result("interpreter", InterpreterEvaluateTest.evaluate())
  println ("Algebra")
  val algebra = Result("algebra", AlgebraEvaluateTest.evaluate())

  val vnames = Seq(OOEvaluateTest.name, AlgebraEvaluateTest.name, VisitorEvaluateTest.name, TriviallyEvaluateTest.name, ExtensibleVisitorEvaluateTest.name, InterpreterEvaluateTest.name)
  val results:Seq[Result] = Seq(oo, algebra, visitor, trivially, extensible, interpreter)

  results.foreach(r => print (r.name + ","))
  println()
  val variations = Seq("e0", "e1", "e2", "e3", "e4", "e5", "e6")
  variations.foreach(mi => {
    print(mi + ",")
    results.foreach(r => print (r.scores(mi).average + ","))
    println()
  }
  )
}
