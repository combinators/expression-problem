package ep.j   /*DI:LD:AI*/

import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.expr.Expression
import ep.domain.{BaseDomain, ModelDomain}
import ep.generator.LanguageIndependentTestGenerator
import org.combinators.templating.twirl.Java

trait TestGenerator extends JavaGenerator with LanguageIndependentTestGenerator {
  val domain: BaseDomain with ModelDomain
  import domain._

  type UnitTest = MethodDeclaration /** Base concept for the representation of a single test case. */

  /**
    * Return properly formatted expected value as a code fragment.
    *
    * This method provides an essential capability that is required, namely, converting an existing
    * test case into a sequence of Java code fragments. The return value is a form of a continuation,
    * that is, it is a function f(exp) => Seq[Statement] which allows us to chain together any number
    * of test cases.
    *
    * The expected value is a pair (TypeRep, Any) which relies on ability to call toString from a code
    * fragment (test.expect._2.toString).
    *
    * However, if you are dealing with more complicated code fragments (i.e., when the value is a list) then
    * you will have to override this method accordingly.
    */
  def expected(test: TestCaseExpectedValue, id: String): (Expression => Seq[Statement]) => Seq[Statement] = continue => {
    continue(Java(test.expect.inst.toString).expression[Expression])
  }


  /** Return sample test cases as methods. */
  def testGenerator: Seq[MethodDeclaration] = Seq.empty

  /** Return MethodDeclaration associated with given test cases. */
  def junitTestMethod(test: TestCase, idx: Int): Seq[Statement] = {
    val id: String = s"v$idx"

    test match {
      case eq: EqualsTestCase =>
        // The expected method takes in a function that will be called by the expected method. Now, the expected
        // method will pass in the expression (which is expected) into this function, and it is the job of that
        // function to return the variable.
        expected(eq, id)(expectedExpr => Java(s"assertEquals($expectedExpr, ${actual(eq.op, eq.inst)});").statements)

      case ne: NotEqualsTestCase =>
        // The expected method takes in a function that will be called by the expected method. Now, the expected
        // method will pass in the expression (which is expected) into this function, and it is the job of that
        // function to return the variable.
        expected(ne, id)(expectedExpr => Java(s"assertNotEquals($expectedExpr, ${actual(ne.op, ne.inst)});").statements)

      case seq: EqualsCompositeTestCase => {
        val x: Expression = actual(seq.ops.head, seq.inst) // HACK: Only works for two-deep
        val y: Expression = dispatch(x, seq.ops.tail.head)
        expected(seq, id)(expectedExpr => Java(s"assertEquals($expectedExpr, $y);").statements)
      }
    }
  }

  /** Return MethodDeclaration associated with given test cases. */
  def testMethod(tests: Seq[TestCase]): Seq[MethodDeclaration] = {
    val stmts = tests.zipWithIndex.flatMap { case (test, idx) => junitTestMethod(test, idx) }

    Java(s"""|public void test() {
             |   ${stmts.mkString("\n")}
             |}""".stripMargin).methodDeclarations
  }
}
