package org.combinators.ep.language.haskell     /*DI:LD:AI*/

import org.combinators.ep.domain.{BaseDomain, ModelDomain}
import org.combinators.ep.generator.LanguageIndependentTestGenerator

trait TestGenerator extends HaskellGenerator with LanguageIndependentTestGenerator {
  val domain: BaseDomain with ModelDomain
  import domain._

  type UnitTest = Seq[Statement]

  /** Return sample test cases as method. */
  def testGenerator: Seq[Haskell] = Seq.empty

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
        }.block   // assertEquals($expectedValue, $actualValue);

      //        expectedBlock.appendDependent { case Seq(expectedValue) =>
      //          actualBlock.appendDependent { case Seq(actualValue) =>
      //            CodeBlockWithResultingExpressions(STMT)()
      //          }
      //        }.block
      //        val source = NoSource
      //        val delta = deltaExprOp(toTargetLanguage(eq.inst), eq.op)
      //        val disp = contextDispatch(source, delta)
      //        expected(eq, idx)(expectedExpr => Seq(new Haskell(s"""test_v$idx = TestCase (assertEqual "${test.getClass.getSimpleName}" ($expectedExpr) $disp)""")))

      //      case seq:EqualsCompositeTestCase =>
      //        val x :Expression = actual(seq.ops.head, seq.inst)   // HACK: Only works for two-deep
      //        val y :Expression = dispatch(x, seq.ops.tail.head)
      //        expected(seq, idx)(expectedExpr => Seq(new Haskell(s"""test_v$idx = TestCase (assertEqual "${test.getClass.getSimpleName}" ($expectedExpr) $y)""")))

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
                contextDispatch(NoSource, deltaExprOp(currentResult, nextOp, params: _*))
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
