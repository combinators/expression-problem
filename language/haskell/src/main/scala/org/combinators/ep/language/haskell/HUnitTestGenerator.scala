package org.combinators.ep.language.haskell    /*DI:LD:AI*/

import org.combinators.ep.domain.{BaseDomain, ModelDomain}
import org.combinators.ep.generator.LanguageIndependentTestGenerator

/**
  * Each evolution has opportunity to enhance the code generators.
  *
  */
trait HUnitTestGenerator extends HaskellGenerator with LanguageIndependentTestGenerator with TestGenerator with PerformanceTestGenerator {
  val domain: BaseDomain with ModelDomain

  /**
    * Represents the sequence of total test cases.
    */
  def testGenerator : Seq[UnitTest] = Seq.empty

//  /** Return JUnit test case associated with these given test cases. */
//  def testMethod(tests:Seq[TestCase]) : Seq[UnitTest] = {
//    val stmts:Seq[HaskellStatement] = testMethod(tests).flatten
//    val structure = tests.zipWithIndex.map(pair => new Haskell(s"""TestLabel "${pair._2}" test_v${pair._2}""")).mkString(",")
//
//    // Awkward. A Test case is a Seq[HaskellStatement]. Returns a sequence for simplicity downstream
//    Seq(Seq(HaskellStatement(s"""|${stmts.mkString("\n")}
//                    |test_all = TestList [ $structure ]
//                    |
//                    |main :: IO Counts
//                    |main  = runTestTT test_all""".stripMargin)))
//  }
}