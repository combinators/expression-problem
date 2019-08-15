package org.combinators.ep.language.haskell     /*DI:LD:AI*/

import org.combinators.ep.domain.BaseDomain
import org.combinators.ep.generator.LanguageIndependentTestGenerator

trait TestGenerator extends HaskellGenerator with LanguageIndependentTestGenerator {
  val domain: BaseDomain with ModelDomain
  import domain._

  type UnitTest = Seq[HaskellStatement]

  /**
    * Traits can override this method to add their test cases to the mix.
    */
  def testMethod(tests:Seq[TestCase]) : Seq[UnitTest] = {
    val stmts = tests.zipWithIndex.flatMap{ case (test, idx) => hunitTestMethod(test, idx) }
    val structure = tests.zipWithIndex.map(pair => new Haskell(s"""TestLabel "${pair._2}" test_v${pair._2}""")).mkString(",")
    //
    // Awkward. A Test case is a Seq[HaskellStatement]. Returns a sequence for simplicity downstream
      Seq(Seq(HaskellStatement(
        s"""|${stmts.mkString("\n")}
            |test_all = TestList [ $structure ]
            |
            |main :: IO Counts
            |main  = runTestTT test_all""".stripMargin)))
}

  /**
    * Override as necessary
    */
  def hunitTestMethod(test:TestCase, idx:Int) : UnitTest = {
    test match {
      case eq: EqualsTestCase =>
        val expectedBlock = toTargetLanguage(eq.expect)
        val parameterBlock =
          eq.params.foldLeft(CodeBlockWithResultingExpressions.empty) {
            case (b, p) => b.appendIndependent(toTargetLanguage(p))
          }

        val actualBlock = parameterBlock.appendDependent(params =>
          actual(eq.op, eq.inst, params: _*)
        )

        expectedBlock.appendDependent { case Seq(expectedValue) =>
          actualBlock.appendDependent { case Seq(actualValue) =>
            CodeBlockWithResultingExpressions(HaskellStatement(s"""test_v$idx = TestCase (assertEqual "${test.getClass.getSimpleName}" $expectedValue $actualValue)"""))()
          }
        }.block
      case seq: EqualsCompositeTestCase =>
        val expectedBlock = toTargetLanguage(seq.expect)
        val actualStartBlock = {
          val parameterBlock =
            seq.ops.head._2.foldLeft(CodeBlockWithResultingExpressions.empty) {
              case (b, p) => b.appendIndependent(toTargetLanguage(p))
            }
          parameterBlock.appendDependent(params =>
            actual(seq.ops.head._1, seq.inst, params: _*)
          )
        }
        val actualBlock = seq.ops.tail.foldLeft(actualStartBlock) { case (currentBlock, (nextOp, nextParams)) =>
          currentBlock.appendDependent { case Seq(currentResult) =>
            val parameterBlock =
              nextParams.foldLeft(CodeBlockWithResultingExpressions.empty) {
                case (b, p) => b.appendIndependent(toTargetLanguage(p))
              }
            parameterBlock.appendDependent(params =>
              CodeBlockWithResultingExpressions(
                contextDispatch(NoSource, dispatchToExpression(currentResult, nextOp, params: _*))
              )
            )
          }
        }

        expectedBlock.appendDependent { case Seq(expectedValue) =>
          actualBlock.appendDependent { case Seq(actualValue) =>
            CodeBlockWithResultingExpressions(HaskellStatement(s"""test_v$idx = TestCase (assertEqual "${test.getClass.getSimpleName}" $expectedValue $actualValue)"""))()
          }
        }.block

      case _ => Seq.empty
    }
  }


}
