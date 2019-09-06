package org.combinators.ep.language.scala     /*DI:LD:AI*/

import org.combinators.ep.domain.BaseDomain
import org.combinators.ep.generator.DomainIndependentTestGenerator

import scala.meta.{Stat, Term}

trait TestGenerator extends ScalaGenerator with DomainIndependentTestGenerator {
  val domain: BaseDomain with ModelDomain
  import domain._

  type UnitTest = Seq[Stat]

  /** Type to use when referring to specific instance. */
  def exprDefine(exp:Inst) : Type = {
    scala.meta.Type.Name(exp.name)
  }

  /** Used when one already has code fragments bound to variables, which are to be used for left and right. */
  def convertRecursive(inst: Binary, left:String, right:String): Expression = {
    val name = inst.name

    Scala(s"new $name($left, $right)").expression
  }

  /** Converts types in test code. */
  def testTypeConverter(ty: TypeRep) : Type = typeConverter(ty)

  /**
    * Traits can override this method to add their test cases to the mix.
    */
  def testMethod(tests:Seq[TestCase]) : Seq[Seq[Stat]] = {
    tests.zipWithIndex.map{ case (test, idx) => scalaTestMethod(test, idx) }
  }

  /** Return MethodDeclaration associated with given test cases. */
  def scalaTestMethod(test: TestCase, idx: Int): Seq[Statement] = {
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
            CodeBlockWithResultingExpressions(Scala(s"assert ($expectedValue == $actualValue)").statement)()
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
            CodeBlockWithResultingExpressions(Scala(s"assert ($unExpectedValue != $actualValue)").statement)()
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
            CodeBlockWithResultingExpressions(Scala(s"assert ($expectedValue == $actualValue)").statement)()
          }
        }.block
    }
  }

}
