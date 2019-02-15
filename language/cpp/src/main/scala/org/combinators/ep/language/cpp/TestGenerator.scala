package org.combinators.ep.language.cpp     /*DI:LD:AI*/

import org.combinators.ep.domain.{BaseDomain, ModelDomain}
import org.combinators.ep.generator.LanguageIndependentTestGenerator

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait TestGenerator extends CPPGenerator with LanguageIndependentTestGenerator {
  val domain: BaseDomain with ModelDomain
  import domain._

  type UnitTest = Seq[CPPElement]

  /**
    * Return sample C++ test cases.
    *
    * A Seq of Seq because each individual test case can be considered to be a (potential) sequence of statements.
    */
  def testGenerator: Seq[Seq[CPPElement]] = Seq.empty

  /** Converts types in test code. */
  def testTypeConverter(ty: TypeRep) : Type = typeConverter(ty)

  /**
    * Performance tests.
    *
    * A Seq of Seq because each individual performance unit can be considered to be a (potential) sequence of statements.
    */
  //def performanceMethod: Seq[Seq[CPPElement]] = Seq.empty

  /** Counter to use for creating artificial variables during testing. */
  var id = 0

  /**
    * Test cases may need to introduce arbitrary variables, which are maintained by this collection
    */
  var variables = collection.mutable.Map[Inst, String]()

  /** Used when one already has code fragments bound to variables, which are to be used for left and right. */
  def convertRecursive(inst: Binary, left:String, right:String): Expression = {
    val name = inst.name
    new CPPExpression(s"new $name($left, $right)")
  }

  /** Register an instance and get its variable identifier. */
  def vars(inst:Inst) : String = {
    if (!variables.contains(inst)) {
      variables = variables + (inst -> s"${inst.name}$id")
      id = id + 1
    }

    variables(inst)
  }

  /**
    * Return properly formatted expected value as a code fragment.
    *
    * This method provides an essential capability that is required, namely, converting an existing
    * test case into a sequence of C++ code fragments. The return value is a form of a continuation,
    * that is, it is a function f(exp) => Seq[Statement] which allows us to chain together any number
    * of test cases.
    *
    * The expected value is a pair (TypeRep, Any) which relies on ability to call toString from a code
    * fragment (test.expect._2.toString).
    *
    * However, if you are dealing with more complicated code fragments (i.e., when the value is a list) then
    * you will have to override this method accordingly.
    */
  def expected(test:TestCaseExpectedValue, id:String) : (CPPElement => Seq[CPPElement]) => Seq[CPPElement] = continue => {
    continue(new CPPExpression(test.expect.inst.toString))
  }

  /**
    * Actual value in a test case.
    *
    * Each basic test case has an instance over which an operation is to be performed. This method
    * returns the inline expression resulting from dispatching operation, op, over the given instance, inst.
    *
    * For more complicated structures, as with lists for example, this method will need to be overridden.
    *
    * Not sure, yet, how to properly pass in variable parameters.
    */
  //def actual(op:Operation, inst:Inst, params:CPPElement*):CPPElement
//
//  def actual(op: domain.Operation, inst: domain.Inst, params: Expression*): CodeBlockWithResultingExpressions = {
//    toTargetLanguage(inst).appendDependent(instExp =>
//      CodeBlockWithResultingExpressions(contextDispatch(NoSource, deltaExprOp(NoSource, instExp.head, op, params: _*)))
//    )
//  }

  /** Convert a test instance into a C++ Expression for instantiating that instance. */
  def rec_convert(inst: Inst): CPPExpression

  /** Convert a test instance into a C++ Expression for instantiating that instance. */
  def convert(inst: Inst): CPPElement = {
    val name = inst.name
    id = id + 1
    inst match {
      case ui: UnaryInst =>
        new CPPStatement(s"$name ${vars(inst)} = $name(&${vars(ui.inner)});")

      case bi: BinaryInst =>
        new CPPStatement(s"$name ${vars(inst)} = $name(&${vars(bi.left)}, &${vars(bi.right)});")

      case exp: AtomicInst =>
        new CPPStatement(
        s"""
           |double val${vars(inst)} = ${exp.ei.inst};
           |$name ${vars(inst)} = $name(&val${vars(inst)});
         """.stripMargin)

      case _ => new CPPStatement(s""" "unknown $name" """)
    }
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
        // The expected method takes in a function that will be called by the expected method. Now, the expected
        // method will pass in the expression (which is expected) into this function, and it is the job of that
        // function to return the variable.
        expected(eq, id)(expectedExpr =>
          Seq(new CPPStatement(s"CHECK_TRUE($expectedExpr == ${actual(eq.op, eq.inst)});")))

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


//      case ne: NotEqualsTestCase =>
//        // The expected method takes in a function that will be called by the expected method. Now, the expected
//        // method will pass in the expression (which is expected) into this function, and it is the job of that
//        // function to return the variable.
//        expected(ne, id)(expectedExpr =>
//          Seq(new CPPElement(s"CHECK_TRUE($expectedExpr != ${actual(ne.op, ne.inst)});")))

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

//      case seq: EqualsCompositeTestCase =>
//        val x: Expression = actual(seq.ops.head, seq.inst) // HACK: Only works for two-deep
//        val y: Expression = dispatch(x, seq.ops.tail.head)
//        expected(seq, id)(expectedExpr => Seq(new CPPElement(s"CHECK_COMPARE($expectedExpr, ==, $y);")))

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
                contextDispatch(NoSource, deltaExprOp(NoSource, currentResult, nextOp, params: _*))
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