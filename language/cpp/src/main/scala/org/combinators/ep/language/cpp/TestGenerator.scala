package ep.cpp     /*DI:LD:AI*/

import ep.domain.{BaseDomain, ModelDomain}
import org.combinators.ep.domain.{BaseDomain, ModelDomain}

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait TestGenerator extends CPPGenerator {
  val domain: BaseDomain with ModelDomain
  import domain._

  /**
    * Return sample C++ test cases.
    *
    * A Seq of Seq because each individual test case can be considered to be a (potential) sequence of statements.
    */
  def testGenerator: Seq[Seq[CPPElement]] = Seq.empty

  /**
    * Performance tests.
    *
    * A Seq of Seq because each individual performance unit can be considered to be a (potential) sequence of statements.
    */
  def performanceMethod: Seq[Seq[CPPElement]] = Seq.empty

  /** Counter to use for creating artificial variables during testing. */
  var id = 0

  /**
    * Test cases may need to introduce arbitrary variables, which are maintained by this collection
    */
  var variables = collection.mutable.Map[AtomicInst, String]()

  /** Used when one already has code fragments bound to variables, which are to be used for left and right. */
  def convertRecursive(inst: Binary, left:String, right:String): Expression = {
    val name = inst.name
    new CPPElement(s"new $name($left, $right)")
  }

  /** Register an instance and get its variable identifier. */
  def vars(inst:AtomicInst) : String = {
    if (!variables.contains(inst)) {
      variables = variables + (inst -> s"${inst.e.name}$id")
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
    continue(new CPPElement(test.expect._2.toString))
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
  def actual(op:Operation, inst:AtomicInst, params:CPPElement*):CPPElement

  /** Convert a test instance into a C++ Expression for instantiating that instance. */
  def rec_convert(inst: AtomicInst): CPPElement

  /** Convert a test instance into a C++ Expression for instantiating that instance. */
  def convert(inst: AtomicInst): CPPElement = {
    val name = inst.e.name
    id = id + 1
    inst match {
      case ui: UnaryInst =>
        new CPPElement(s"$name ${vars(inst)} = $name(&${vars(ui.inner)});")

      case bi: BinaryInst =>
        new CPPElement(s"$name ${vars(inst)} = $name(&${vars(bi.left)}, &${vars(bi.right)});")

      case exp: AtomicInst =>
        new CPPElement(
        s"""
           |double val${vars(inst)} = ${exp.i.get};
           |$name ${vars(inst)} = $name(&val${vars(inst)});
         """.stripMargin)

      case _ => new CPPElement(s""" "unknown $name" """)
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
          Seq(new CPPElement(s"CHECK_TRUE($expectedExpr == ${actual(eq.op, eq.inst)});")))

      case ne: NotEqualsTestCase =>
        // The expected method takes in a function that will be called by the expected method. Now, the expected
        // method will pass in the expression (which is expected) into this function, and it is the job of that
        // function to return the variable.
        expected(ne, id)(expectedExpr =>
          Seq(new CPPElement(s"CHECK_TRUE($expectedExpr != ${actual(ne.op, ne.inst)});")))

      case seq: EqualsCompositeTestCase =>
        val x: Expression = actual(seq.ops.head, seq.inst) // HACK: Only works for two-deep
        val y: Expression = dispatch(x, seq.ops.tail.head)
        expected(seq, id)(expectedExpr => Seq(new CPPElement(s"CHECK_COMPARE($expectedExpr, ==, $y);")))

      case _ => Seq.empty
    }
  }

  /**
    * Traits can override this method to add their test cases to the mix.
    */
  def testMethod(tests:Seq[TestCase]) : Seq[Seq[Statement]] = {
    tests.zipWithIndex.map{ case (test, idx) => cppUnitTestMethod(test, idx) }
  }

  /** Combine all test cases together into a single JUnit 3.0 TestSuite class. */
  def generateSuite(pkg: Option[String], model: Option[Model] = None): Seq[CPPFile] = {
    val tests = testGenerator ++ performanceMethod
    val allTests = tests.zipWithIndex.map{ case (t, num) =>

      new CPPElement(
            s"""
               |TEST_GROUP(TestGroup$num)
               |{
               |};
               |
              |TEST(TestGroup$num, a$num)
               |{
               |   ${t.mkString("\n")}
               |}""".stripMargin
      )
    }

    // include performance timing code
    val sa = new StandAlone("test_e0",
      s"""
         |#include <sys/time.h>
         |long diffTimer (struct timeval *before, struct timeval *after) {
         |  long ds = after->tv_sec - before->tv_sec;
         |  long uds = after->tv_usec - before->tv_usec;
         |
         |  /* if secs are same, then return simple delta */
         |  if (ds == 0) {
         |    return uds;
         |  }
         |
         |  /* ok, so we are turned over. Something like: */
         |  /* before: 1179256745 744597                  */
         |  /* after:  1179256746 73514                   */
         |
         |  /* if we have 'turned over' then account properly */
         |  if (uds < 0) {
         |    ds = ds - 1;
         |    uds += 1000000;
         |  }
         |
         |  return 1000000*ds + uds;
         |}
         |
         |${allTests.mkString("\n")}
         |
         |int main(int ac, char** av)
         |{
         |  MemoryLeakWarningPlugin::turnOffNewDeleteOverloads();
         |  return CommandLineTestRunner::RunAllTests(ac, av);
         |}""".stripMargin.split("\n")
    )

    sa.addHeader(Seq(
      """#include "CppUTest/TestHarness.h" """,
      """#include "CppUTest/SimpleString.h" """,
      """#include "CppUTest/PlatformSpecificFunctions.h" """,
      """#include "CppUTest/TestMemoryAllocator.h" """,
      """#include "CppUTest/MemoryLeakDetector.h" """,
      """#include "CppUTest/CommandLineTestRunner.h" """))

    Seq(sa)
  }
}