package org.combinators.ep.language.haskell    /*DI:LD:AI*/

import org.combinators.ep.domain.{BaseDomain, ModelDomain}
import org.combinators.ep.generator.LanguageIndependentTestGenerator

/**
  * Each evolution has opportunity to enhance the code generators.
  *
  */
trait HUnitTestGenerator extends HaskellGenerator with LanguageIndependentTestGenerator with TestGenerator with PerformanceTestGenerator {
  val domain: BaseDomain with ModelDomain

  import domain._

  /** Create multiple Haskell files for test cases. */
  def generateSuite(model: Option[Model] = None): Seq[HaskellWithPath]

  /** Convert the given atomic instance. */
//  def convert(inst:Inst) : Haskell

//  /**
//    * Return properly formatted expected value as a string.
//    */
//  def expected(test:TestCaseExpectedValue, id:Int) : (Haskell => Seq[Haskell]) => Seq[Haskell] = continue => {
//    continue(new Haskell(test.expect.inst.toString))
//  }

//  /**
//    * Actual value in a test case.
//    *
//    * Each basic test case has an instance over which an operation is to be performed. This method
//    * returns the inline expression resulting from dispatching operation, op, over the given instance, inst.
//    *
//    * For more complicated structures, as with lists for example, this method will need to be overridden.
//    */
//  def actual(op:Operation, inst:Inst):Haskell = dispatch(convert(inst), op)

//  /**
//    * Actual value in a test case.
//    *
//    * Each basic test case has an instance over which an operation is to be performed. This method
//    * returns the inline expression resulting from dispatching operation, op, over the given instance, inst.
//    *
//    */
//  def actual(op: domain.Operation, inst: domain.Inst, params: Expression*): CodeBlockWithResultingExpressions = {
//    toTargetLanguage(inst).appendDependent(instExp =>
//      CodeBlockWithResultingExpressions(contextDispatch(NoSource, deltaExprOp(instExp.head, op, params: _*)))
//    )
//  }
//  // THIS SHOULD BE REPLACED ^^^^^^^^^^^^^^^^^ DO I REALLY NEED THIS &&&&&&&

  /**
    * Traits can override this method to add their test cases to the mix.
    */
  def testMethod(tests:Seq[TestCase]) : Seq[UnitTest] = {
    tests.zipWithIndex.map{ case (test, idx) => hunitTestMethod(test, idx) }
  }


  /** Return JUnit test case associated with these given test cases. */
  def hunitMethod(tests:Seq[TestCase]) : Haskell = {
    val stmts:Seq[HaskellStatement] = tests.zipWithIndex.flatMap(pair => hunitTestMethod(pair._1, pair._2))
    val structure = tests.zipWithIndex.map(pair => new Haskell(s"""TestLabel "${pair._2}" test_v${pair._2}""")).mkString(",")

    new Haskell(s"""|${stmts.mkString("\n")}
                    |test_all = TestList [ $structure ]
                    |
                    |main :: IO Counts
                    |main  = runTestTT test_all""".stripMargin)
  }
}