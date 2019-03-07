package org.combinators.ep.language.haskell.performance

/**
  * Code exists to launch performance analysis of code generation of Java solutions. Not part of the
  * standard code generator framework.
  */
import System.nanoTime

import org.combinators.ep.domain.WithDomain
import org.combinators.ep.domain.math.MathDomain
import org.combinators.ep.generator.{LanguageIndependentGenerator, LanguageIndependentTestGenerator}
import org.combinators.ep.language.haskell.alacarte.{ALaCarteGenerator, ALaCarteTestGenerator}
import org.combinators.ep.language.haskell._
import org.combinators.ep.language.haskell.grow.{GrowGenerator, GrowTestGenerator}
import org.combinators.ep.language.haskell.straight.{StraightGenerator, StraightTestGenerator}
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
object ALaCarteTest  {
  def name = Some("oo")

  def evaluate() : Map[String,Score] = {

    val e0 = new BaseTest("e0") {
      override val gen = new WithDomain(MathDomain) with ALaCarteGenerator with ALaCarteTestGenerator with e0
    }
    val e1 = new BaseTest("e1") {
      override val gen = new WithDomain(MathDomain) with ALaCarteGenerator with ALaCarteTestGenerator with e0 with e1
    }
    val e2 = new BaseTest("e2") {
      override val gen = new WithDomain(MathDomain) with ALaCarteGenerator with ALaCarteTestGenerator with e0 with e1 with e2
    }
    val e3 = new BaseTest("e3") {
      override val gen = new WithDomain(MathDomain) with ALaCarteGenerator with ALaCarteTestGenerator with e0 with e1 with e2 with e3
    }
    val e4 = new BaseTest("e4") {
      override val gen = new WithDomain(MathDomain) with ALaCarteGenerator with ALaCarteTestGenerator with e0 with e1 with e2 with e3 with e4
    }
    val e5 = new BaseTest("e5") {
      override val gen = new WithDomain(MathDomain) with ALaCarteGenerator with ALaCarteTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
    }
    val e6 = new BaseTest("e6") {
      override val gen = new WithDomain(MathDomain) with ALaCarteGenerator with ALaCarteTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
    }

    Sample.process(name, Seq(e0, e1, e2, e3, e4, e5, e6))
  }
}

// might be easier way to do this...
object GrowTest extends App {
  def name = Some("trivially")

  def evaluate() : Map[String,Score] = {

    val e0 = new BaseTest("e0") {
      override val gen = new WithDomain(MathDomain) with GrowGenerator with GrowTestGenerator with e0
    }
    val e1 = new BaseTest("e1") {
      override val gen = new WithDomain(MathDomain) with GrowGenerator with GrowTestGenerator with e0 with e1
    }
    val e2 = new BaseTest("e2") {
      override val gen = new WithDomain(MathDomain) with GrowGenerator with GrowTestGenerator with e0 with e1 with e2
    }
    val e3 = new BaseTest("e3") {
      override val gen = new WithDomain(MathDomain) with GrowGenerator with GrowTestGenerator with e0 with e1 with e2 with e3
    }
    val e4 = new BaseTest("e4") {
      override val gen = new WithDomain(MathDomain) with GrowGenerator with GrowTestGenerator with e0 with e1 with e2 with e3 with e4
    }
    val e5 = new BaseTest("e5") {
      override val gen = new WithDomain(MathDomain) with GrowGenerator with GrowTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
    }
    val e6 = new BaseTest("e6") {
      override val gen = new WithDomain(MathDomain) with GrowGenerator with GrowTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
    }

    Sample.process(name, Seq(e0, e1, e2, e3, e4, e5, e6))
  }
}


// might be easier way to do this...
object StraightTest extends App {

  def name = Some("algebra")

  def evaluate() : Map[String,Score] = {

    val e0 = new BaseTest("e0") {
      override val gen = new WithDomain(MathDomain) with StraightGenerator with StraightTestGenerator with e0
    }
    val e1 = new BaseTest("e1") {
      override val gen = new WithDomain(MathDomain) with StraightGenerator with StraightTestGenerator with e0 with e1
    }
    val e2 = new BaseTest("e2") {
      override val gen = new WithDomain(MathDomain) with StraightGenerator with StraightTestGenerator with e0 with e1 with e2
    }
    val e3 = new BaseTest("e3") {
      override val gen = new WithDomain(MathDomain) with StraightGenerator with StraightTestGenerator with e0 with e1 with e2 with e3
    }
    val e4 = new BaseTest("e4") {
      override val gen = new WithDomain(MathDomain) with StraightGenerator with StraightTestGenerator with e0 with e1 with e2 with e3 with e4
    }
    val e5 = new BaseTest("e5") {
      override val gen = new WithDomain(MathDomain) with StraightGenerator with StraightTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
    }
    val e6 = new BaseTest("e6") {
      override val gen = new WithDomain(MathDomain) with StraightGenerator with StraightTestGenerator with e0 with e1 with e2 with e3 with e4 with e5 with e6
    }

    Sample.process(name, Seq(e0, e1, e2, e3, e4, e5, e6))
  }
}


object RunAll extends App {
  println ("Generating data...")

  case class Result(name:String, scores:Map[String,Score])

  //
  println("Calibrating") // Burn first one
  StraightTest.evaluate()

  println ("Straight")
  val straight      = Result("straight", StraightTest.evaluate())
  println ("ALaCarte")
  val alacarte = Result("alacarte", ALaCarteTest.evaluate())
  println ("Grow")
  val grow = Result("grow", GrowTest.evaluate())

  val vnames = Seq(StraightTest.name, ALaCarteTest.name, GrowTest.name)
  val results:Seq[Result] = Seq(straight, alacarte, grow)

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
