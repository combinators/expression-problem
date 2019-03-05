package org.combinators.ep.language.gj      /*DI:LD:AI*/

import java.nio.file.Paths

import org.combinators.ep.domain.{BaseDomain, ModelDomain}
import org.combinators.ep.generator.LanguageIndependentTestGenerator

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait TestGenerator extends GJGenerator with LanguageIndependentTestGenerator {
  val domain: BaseDomain with ModelDomain
  import domain._

  type UnitTest = Seq[GJStatement]

  /**
    * In test cases, languages are instantiated from which new is called.
    */
  def testDispatch(expr:GJ, op:domain.Operation, params:GJ*) : GJ = {
    val args:String = params.mkString(",")
    GJ(s"""$expr.visit(l.new ${op.concept}())""")
  }

  /**
    * Represents the sequence of total test cases.
    */
  def testGenerator : Seq[UnitTest] = Seq.empty

  /**
    * Traits can override this method to add their test cases to the mix.
    */
  def testMethod(tests:Seq[TestCase]) : Seq[UnitTest] = {
    tests.zipWithIndex.map{ case (test, idx) => gjTestMethod(test, idx) }
  }

  /** Return MethodDeclaration associated with given test cases. */
  def gjTestMethod(test: TestCase, idx: Int): Seq[Statement] = {
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
            CodeBlockWithResultingExpressions(GJStatement(s"assertEquals($expectedValue, $actualValue);"))()
          }
        }.block

      case ne: NotEqualsTestCase =>
        val unExpectedBlock = toTargetLanguage(ne.expect)
        val parameterBlock =
          ne.params.foldLeft(CodeBlockWithResultingExpressions.empty) {
            case (b, p) => b.appendIndependent(toTargetLanguage(p))
          }
        val actualBlock =
          parameterBlock.appendDependent(params =>
            actual(ne.op, ne.inst, params: _*)
          )

        unExpectedBlock.appendDependent { case Seq(unExpectedValue) =>
          actualBlock.appendDependent { case Seq(actualValue) =>
            CodeBlockWithResultingExpressions(GJStatement(s"assertNotEquals($unExpectedValue, $actualValue);"))()
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
                contextDispatch(NoSource, deltaExprOp(currentResult, nextOp, params: _*))
              )
            )
          }
        }

        expectedBlock.appendDependent { case Seq(expectedValue) =>
          actualBlock.appendDependent { case Seq(actualValue) =>
            CodeBlockWithResultingExpressions(GJStatement(s"assertEquals($expectedValue, $actualValue);"))()
          }
        }.block
    }
  }

  /** Combine all test cases together into a single JUnit 3.0 TestSuite class. */
  override def generateSuite(pkg: Option[String]): Seq[GJWithPath] = {
    var num: Int = 0

    val methods = testGenerator.map(md => {
      num = num + 1
      GJ(s"""|
             |static public void test$num() {
             | $md
             |}""".stripMargin).getCode
      }).mkString("\n")

    val invocations = (1 to num).map(d => s"test$d();").mkString("\n")
    val code = GJ(
      s"""
         |final class TestSuite {
         |  $methods
         |  static public void main (String[] args) {
         |    $invocations
         |  }
         |}
       """.stripMargin)

    Seq(GJWithPath(code, Paths.get(s"TestSuite.gj")))
  }
}