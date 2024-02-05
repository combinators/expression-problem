package org.combinators.ep.language.java.performance

/**
 * Code exists to launch performance analysis of code generation of Java solutions. Not part of the
 * standard code generator framework.
 */
import System.nanoTime

import org.combinators.ep.domain.WithDomain
import org.combinators.ep.domain.math.MathDomain
import org.combinators.ep.generator.{LanguageIndependentGenerator, LanguageIndependentTestGenerator}
import org.combinators.ep.language.java._
import org.combinators.ep.language.java.algebra.AlgebraGenerator
import org.combinators.ep.language.java.extensibleVisitor.ExtensibleVisitorGenerator
import org.combinators.ep.language.java.interpreter.InterpreterGenerator
import org.combinators.ep.language.java.oo._
import org.combinators.ep.language.java.trivially.TriviallyGenerator
import org.combinators.ep.language.java.visitor.VisitorGenerator
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

  def process(name:Option[String], tests:Seq[BaseTest]) : Map[String,Score] = {

    // get thing started. Burn this time to ensure we don't get biased by first run.
    tests.foreach(t => t.generatedCode(name))

    // now run the real tests
    tests.map(t => t.id -> sample({t.generatedCode(name)})).toMap[String,Score]
  }
}

abstract class BaseTest(val id:String) {
  val gen: WithDomain[MathDomain] with LanguageIndependentGenerator with LanguageIndependentTestGenerator

  // time the synthesis of the generated code plus test suites
  def generatedCode(pkg:Option[String]): Long = {
    val now = nanoTime
    gen.generatedCode() ++ gen.generateSuite(pkg)
    nanoTime - now
  }
}


// might be easier way to do this...
object OOEvaluateTest  {
  def name = Some("oo")

  def evaluate() : Map[String,Score] = {

    val e0 = new BaseTest("e0") {
      override val gen = new WithDomain(MathDomain) with OOGenerator with JUnitTestGenerator with e0
    }
    val e1 = new BaseTest("e1") {
      override val gen = new WithDomain(MathDomain) with OOGenerator with JUnitTestGenerator with e0 with e1
    }
    val e2 = new BaseTest("e2") {
      override val gen = new WithDomain(MathDomain) with OOGenerator with JUnitTestGenerator with e0 with e1 with e2
    }
    val e3 = new BaseTest("e3") {
      override val gen = new WithDomain(MathDomain) with OOGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3
    }
    val e4 = new BaseTest("e4") {
      override val gen = new WithDomain(MathDomain) with OOGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4
    }
    val e5 = new BaseTest("e5") {
      override val gen = new WithDomain(MathDomain) with OOGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
    }
    val e6 = new BaseTest("e6") {
      override val gen = new WithDomain(MathDomain) with OOGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
    }

    Sample.process(name, Seq(e0, e1, e2, e3, e4, e5, e6))
  }
}

// might be easier way to do this...
object TriviallyEvaluateTest extends App {
  def name = Some("trivially")

  def evaluate() : Map[String,Score] = {

    val e0 = new BaseTest("e0") {
      override val gen = new WithDomain(MathDomain) with TriviallyGenerator with JUnitTestGenerator with e0
    }
    val e1 = new BaseTest("e1") {
      override val gen = new WithDomain(MathDomain) with TriviallyGenerator with JUnitTestGenerator with e0 with e1
    }
    val e2 = new BaseTest("e2") {
      override val gen = new WithDomain(MathDomain) with TriviallyGenerator with JUnitTestGenerator with e0 with e1 with e2
    }
    val e3 = new BaseTest("e3") {
      override val gen = new WithDomain(MathDomain) with TriviallyGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3
    }
    val e4 = new BaseTest("e4") {
      override val gen = new WithDomain(MathDomain) with TriviallyGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4
    }
    val e5 = new BaseTest("e5") {
      override val gen = new WithDomain(MathDomain) with TriviallyGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
    }
    val e6 = new BaseTest("e6") {
      override val gen = new WithDomain(MathDomain) with TriviallyGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
    }

    Sample.process(name, Seq(e0, e1, e2, e3, e4, e5, e6))
  }
}


// might be easier way to do this...
object AlgebraEvaluateTest extends App {

  def name = Some("algebra")

  def evaluate() : Map[String,Score] = {

    val e0 = new BaseTest("e0") {
      override val gen = new WithDomain(MathDomain) with AlgebraGenerator with JUnitTestGenerator with e0
    }
    val e1 = new BaseTest("e1") {
      override val gen = new WithDomain(MathDomain) with AlgebraGenerator with JUnitTestGenerator with e0 with e1
    }
    val e2 = new BaseTest("e2") {
      override val gen = new WithDomain(MathDomain) with AlgebraGenerator with JUnitTestGenerator with e0 with e1 with e2
    }
    val e3 = new BaseTest("e3") {
      override val gen = new WithDomain(MathDomain) with AlgebraGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3
    }
    val e4 = new BaseTest("e4") {
      override val gen = new WithDomain(MathDomain) with AlgebraGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4
    }
    val e5 = new BaseTest("e5") {
      override val gen = new WithDomain(MathDomain) with AlgebraGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
    }
    val e6 = new BaseTest("e6") {
      override val gen = new WithDomain(MathDomain) with AlgebraGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
    }

    Sample.process(name, Seq(e0, e1, e2, e3, e4, e5, e6))
  }
}


// might be easier way to do this...
object InterpreterEvaluateTest extends App {

  def name = Some("interpreter")

  def evaluate() : Map[String,Score] = {
    val e0 = new BaseTest("e0") {
      override val gen = new WithDomain(MathDomain) with InterpreterGenerator with JUnitTestGenerator with e0
    }
    val e1 = new BaseTest("e1") {
      override val gen = new WithDomain(MathDomain) with InterpreterGenerator with JUnitTestGenerator with e0 with e1
    }
    val e2 = new BaseTest("e2") {
      override val gen = new WithDomain(MathDomain) with InterpreterGenerator with JUnitTestGenerator with e0 with e1 with e2
    }
    val e3 = new BaseTest("e3") {
      override val gen = new WithDomain(MathDomain) with InterpreterGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3
    }
    val e4 = new BaseTest("e4") {
      override val gen = new WithDomain(MathDomain) with InterpreterGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4
    }
    val e5 = new BaseTest("e5") {
      override val gen = new WithDomain(MathDomain) with InterpreterGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
    }
    val e6 = new BaseTest("e6") {
      override val gen = new WithDomain(MathDomain) with InterpreterGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
    }

    Sample.process(name, Seq(e0, e1, e2, e3, e4, e5, e6))
  }
}


// might be easier way to do this...
object VisitorEvaluateTest extends App {
  def name = Some("visitor")

  def evaluate() : Map[String,Score] = {
    val e0 = new BaseTest("e0") {
      override val gen = new WithDomain(MathDomain) with VisitorGenerator with JUnitTestGenerator with e0
    }
    val e1 = new BaseTest("e1") {
      override val gen = new WithDomain(MathDomain) with VisitorGenerator with JUnitTestGenerator with e0 with e1
    }
    val e2 = new BaseTest("e2") {
      override val gen = new WithDomain(MathDomain) with VisitorGenerator with JUnitTestGenerator with e0 with e1 with e2
    }
    val e3 = new BaseTest("e3") {
      override val gen = new WithDomain(MathDomain) with VisitorGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3
    }
    val e4 = new BaseTest("e4") {
      override val gen = new WithDomain(MathDomain) with VisitorGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4
    }
    val e5 = new BaseTest("e5") {
      override val gen = new WithDomain(MathDomain) with VisitorGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
    }
    val e6 = new BaseTest("e6") {
      override val gen = new WithDomain(MathDomain) with VisitorGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
    }

    Sample.process(name, Seq(e0, e1, e2, e3, e4, e5, e6))
  }
}


// might be easier way to do this...
object ExtensibleVisitorEvaluateTest extends App {

  def name = Some("extensible")

  def evaluate() : Map[String,Score] = {
    val e0 = new BaseTest("e0") {
      override val gen = new WithDomain(MathDomain) with ExtensibleVisitorGenerator with JUnitTestGenerator with e0
    }
    val e1 = new BaseTest("e1") {
      override val gen = new WithDomain(MathDomain) with ExtensibleVisitorGenerator with JUnitTestGenerator with e0 with e1
    }
    val e2 = new BaseTest("e2") {
      override val gen = new WithDomain(MathDomain) with ExtensibleVisitorGenerator with JUnitTestGenerator with e0 with e1 with e2
    }
    val e3 = new BaseTest("e3") {
      override val gen = new WithDomain(MathDomain) with ExtensibleVisitorGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3
    }
    val e4 = new BaseTest("e4") {
      override val gen = new WithDomain(MathDomain) with ExtensibleVisitorGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4
    }
    val e5 = new BaseTest("e5") {
      override val gen = new WithDomain(MathDomain) with ExtensibleVisitorGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
    }
    val e6 = new BaseTest("e6") {
      override val gen = new WithDomain(MathDomain) with ExtensibleVisitorGenerator with JUnitTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
    }

    Sample.process(name, Seq(e0, e1, e2, e3, e4, e5, e6))
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
