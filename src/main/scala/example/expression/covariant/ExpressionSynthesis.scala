package example.expression.covariant

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.expr.SimpleName
import com.github.javaparser.ast.stmt.Statement
import example.expression.j.Operators
import expression._
import expression.data.Eval
import expression.history.History
import expression.instances.{UnitSuite, UnitTest}
import org.combinators.templating.twirl.Java
import shared.compilation.OperationDependency

import scala.collection.JavaConverters._

/** Future work to sanitize combinators to be independent of Exp. */
trait ExpressionSynthesis extends Operators with OperationDependency {

  def testCaseGenerator(op:Operation, identifier:SimpleName, tc: UnitTest) : Seq[Statement]

  def history:History

  /**
    * Construct JUnit test cases for each registered expression.
    *
    * Within the test case method, each of the registered operations are evaluated to confirm the expect value
    * matches the actual value.
    *
    * Sure would like to convert this into a generator, since then (in covariant) I would generate the appropriate
    * instantiation based on the domain model available at the time. Note that this might still be possible,
    * assuming the representationCodeGenerators has access to the top-level domain model.
    */
  def Driver (allTests:UnitSuite): CompilationUnit = {
      // each test creates a public void testXXX method, inside of which, each sub-part is evaluated.
      var testNumber = 0
      val allGen:String = allTests.iterator.asScala.map(tst => {

        // build up MustHave interfaces, from the available operations. This is maximally conservative
        // and likely more than necessary
        // be sure to include all dependent operations as well. TODO: THIS SHOULD BE TRANSITIVE
        // BUT WE KEEP SIMPLE FOR NOW.
        val mustHave:List[Operation] = tst.operations.asScala // history.flatten.ops.asScala
            .filterNot(op => op.equals (new Eval))   //  op.getClass.getSimpleName.equals("Eval"
            .foldLeft(List[Operation]())((collect, op) => collect ++ List(op) ++ dependency(op) )

        // build up expression for which all tests are defined.
        // Note that we might need model in addition to test case itself to properly construct this
        // either:   Add add = new AddFinal(new LitFinal(7), new LitFinal(4));
        val testDomain = new TestCaseCodeGenerators(history, tst, mustHave)
        val code:Option[com.github.javaparser.ast.expr.Expression] = testDomain.instanceGenerators(tst.expression)

        testNumber = testNumber+1
        var subTypes:String = testDomain.computeSubTypes()
        if (subTypes.length == 0) {
          subTypes = "Exp"   // need to have in case all subtypes vanish.
        }
        val init:String = s"""$subTypes exp$testNumber = ${code.get.toString};"""
        val ident:SimpleName = Java(s"exp$testNumber").simpleName()

        val blocks:String = tst.iterator().asScala.flatMap(tc => testCaseGenerator(tc.op, ident, tc)).mkString("\n")

        val codeBlock:String = s"""|public void testEval$testNumber() {
                                   |  $init
                                   |  $blocks
                                   |}""".stripMargin
        Java(codeBlock).methodDeclarations().mkString("\n")
      }).mkString("\n")

      Java(s"""|package ep;
               |import junit.framework.TestCase;
               |public class TestSuite extends TestCase {
               |    $allGen
               |}""".stripMargin).compilationUnit()
    }
}