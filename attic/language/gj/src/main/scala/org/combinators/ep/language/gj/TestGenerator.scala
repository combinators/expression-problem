package org.combinators.ep.language.gj      /*DI:LD:AI*/

import org.combinators.ep.domain.BaseDomain
import org.combinators.ep.generator.DomainIndependentTestGenerator

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait TestGenerator extends GJGenerator with DomainIndependentTestGenerator {
  val domain: BaseDomain with ModelDomain
  import domain._

  type UnitTest = Seq[GJStatement]

  /** Converts types in test code. */
  def testTypeConverter(ty: TypeRep) : Type = {
    val last = "Lang_" + getModel.name
    ty match {
      case domain.baseTypeRep => new GJType(s"$last.${domain.baseTypeRep.name}")
      case _ => typeConverter(ty)
    }
  }

  /**
    * Represents the sequence of total test cases.
    */
  def testGenerator : Seq[UnitTest] = Seq.empty

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
            CodeBlockWithResultingExpressions(GJStatement(s"""System.out.println ($expectedValue + " should equal " + $actualValue);"""))()
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
            CodeBlockWithResultingExpressions(GJStatement(s"""System.out.println ($unExpectedValue + " should NOT equal " + $actualValue);"""))()
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
            CodeBlockWithResultingExpressions(GJStatement(s"""System.out.println ($expectedValue + " should equal " + $actualValue);"""))()
          }
        }.block
    }
  }

  /** Return MethodDeclaration associated with given test cases. */
  def testMethod(tests: Seq[TestCase]): Seq[UnitTest] = {
    val stmts = tests.zipWithIndex.flatMap { case (test, idx) => gjTestMethod(test, idx) }
    if (stmts.isEmpty) {
      Seq.empty
    } else {
      Seq(Seq(GJStatement(
        s"""|public static void main(String[] args) {
            |   ${stmts.mkString("\n")}
            |}""".stripMargin)))
    }
  }

}