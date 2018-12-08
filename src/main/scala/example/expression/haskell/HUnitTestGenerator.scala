package example.expression.haskell    /*DI:LD:AI*/

import example.expression.domain.{BaseDomain, ModelDomain}

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait HUnitTestGenerator extends HaskellGenerator {
  val domain: BaseDomain with ModelDomain

  import domain._

  /** Return sample HUnit test cases. */
  def testGenerator: Seq[Haskell] = Seq.empty

  /** Create multiple Haskell files for test cases. */
  def generateSuite(model: Option[Model] = None): Seq[HaskellWithPath]

  /** Convert the given atomic instance. */
  def convert(inst:AtomicInst) : Haskell

  /**
    * Return properly formatted expected value as a string.
    * TODO: Future plan to return a proper class which can be refined with strategy rather than pure continuation
    */
  def expected(test:TestCase, id:String) : (Haskell => Seq[Haskell]) => Seq[Haskell] = continue => {
    continue(new Haskell(test.expect._2.toString))
  }

  /** Actual value in a test case. */
  def actual(test:TestCase):Haskell = dispatch(convert(test.inst), test.op)

  /** Return JUnit test case associated with these given test cases. */
  def hunitMethod(tests:Seq[TestCase]) : Haskell = {
    val stmts:Seq[Haskell] = tests.zipWithIndex.flatMap(pair => {
      val test = pair._1
      val idx = pair._2

      val id:String = s"v$idx"

      // test_e3_1 = TestCase (assertEqual "NegCheck-Eval" (0-5.0) (${Eval.name} n1))
      val disp = dispatch(convert(test.inst), test.op)
      expected(test, id)(expectedExpr => Seq(new Haskell(s"""test_$id = TestCase (assertEqual "${test.getClass.getSimpleName}" ($expectedExpr) $disp)""")))
    })

    val structure = tests.zipWithIndex.map(pair => {
      val idx = pair._2
      new Haskell(s"""TestLabel "$idx" test_v$idx""")
    }).mkString(",")

    new Haskell(s"""|${stmts.mkString("\n")}
                    |test_all = TestList [ $structure ]
                    |
                    |main :: IO Counts
                    |main  = runTestTT test_all""".stripMargin)
  }
}