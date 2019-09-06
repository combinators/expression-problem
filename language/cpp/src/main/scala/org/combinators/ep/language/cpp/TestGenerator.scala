package org.combinators.ep.language.cpp     /*DI:LD:AI*/

import org.combinators.ep.domain.BaseDomain
import org.combinators.ep.generator.DomainIndependentTestGenerator

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait TestGenerator extends CPPGenerator with DomainIndependentTestGenerator {
  val domain: BaseDomain with ModelDomain
  import domain._

  type UnitTest = Seq[CPPElement]

  /** Converts types in test code. */
  def testTypeConverter(ty: TypeRep) : Type = typeConverter(ty)

  /** Counter to use for creating artificial variables during testing. */
  var id = 0

  /**
    * Test cases may need to introduce arbitrary variables, which are maintained by this collection
    */
  var variables = collection.mutable.Map[Inst, String]()

  /** Register an instance and get its variable identifier. */
  def vars(inst:Inst) : String = {
    if (!variables.contains(inst)) {
      variables = variables + (inst -> s"${inst.name}$id")
      id = id + 1
    }

    variables(inst)
  }

  /**
    * Prepare default test cases for [[BaseDomain.EqualsTestCase]],
    * [[BaseDomain.NotEqualsTestCase]], [[BaseDomain.EqualsCompositeTestCase]].
    *
    * Override as necessary to add different test case types.
    */
  def cppUnitTestMethod(test:TestCase, idx:Int) : Seq[Statement] = {
    val id: String = s"v$idx"

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
            CodeBlockWithResultingExpressions(new CPPStatement(s"CHECK_TRUE($expectedValue == $actualValue);"))()
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
            CodeBlockWithResultingExpressions(new CPPStatement(s"CHECK_TRUE($unExpectedValue != $actualValue);"))()
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
            CodeBlockWithResultingExpressions(new CPPStatement(s"CHECK_COMPARE($expectedValue, ==, $actualValue);"))()
          }
        }.block

      case _ => Seq.empty
    }
  }

  /**
    * Traits can override this method to add their test cases to the mix.
    */
  def testMethod(tests:Seq[TestCase]) : Seq[Seq[Statement]] = {
    tests.zipWithIndex.map{ case (test, idx) => cppUnitTestMethod(test, idx) }
  }

}