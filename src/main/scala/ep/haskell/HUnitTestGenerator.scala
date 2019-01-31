package ep.haskell    /*DI:LD:AI*/

import ep.domain.{BaseDomain, ModelDomain}

/**
  * Each evolution has opportunity to enhance the code generators.
  *
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
    */
  def expected(test:TestCaseExpectedValue, id:Int) : (Haskell => Seq[Haskell]) => Seq[Haskell] = continue => {
    continue(new Haskell(test.expect._2.toString))
  }

  /**
    * Actual value in a test case.
    *
    * Each basic test case has an instance over which an operation is to be performed. This method
    * returns the inline expression resulting from dispatching operation, op, over the given instance, inst.
    *
    * For more complicated structures, as with lists for example, this method will need to be overridden.
    */
  def actual(op:Operation, inst:AtomicInst):Haskell = dispatch(convert(inst), op)

  /**
    * Override as necessary
    */
  def hunitTestMethod(test:TestCase, idx:Int) : Seq[Haskell] = {
    test match {
      case eq: EqualsTestCase =>

        val source = NoSource()
        val delta = deltaExprOp(source, convert(eq.inst), eq.op)
        val disp = contextDispatch(source, delta)
        expected(eq, idx)(expectedExpr => Seq(new Haskell(s"""test_v$idx = TestCase (assertEqual "${test.getClass.getSimpleName}" ($expectedExpr) $disp)""")))

      case seq:EqualsCompositeTestCase =>
        val x :Expression = actual(seq.ops.head, seq.inst)   // HACK: Only works for two-deep
        val y :Expression = dispatch(x, seq.ops.tail.head)
        expected(seq, idx)(expectedExpr => Seq(new Haskell(s"""test_v$idx = TestCase (assertEqual "${test.getClass.getSimpleName}" ($expectedExpr) $y)""")))

      case _ => Seq.empty
    }
  }

  /** Return JUnit test case associated with these given test cases. */
  def hunitMethod(tests:Seq[TestCase]) : Haskell = {
    val stmts:Seq[Haskell] = tests.zipWithIndex.flatMap(pair => hunitTestMethod(pair._1, pair._2))
    val structure = tests.zipWithIndex.map(pair => new Haskell(s"""TestLabel "${pair._2}" test_v${pair._2}""")).mkString(",")

    new Haskell(s"""|${stmts.mkString("\n")}
                    |test_all = TestList [ $structure ]
                    |
                    |main :: IO Counts
                    |main  = runTestTT test_all""".stripMargin)
  }
}