package example.expression.covariant

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.stmt.Statement
import expression._
import expression.data.Eval
import expression.extensions.{Collect, PrettyP}
import expression.history.History
import expression.instances.{Instance, Lit, UnitSuite}
import expression.operations.SimplifyExpr
import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.Type
import org.combinators.templating.twirl.Java

import scala.collection.JavaConverters._

/** Future work to sanitize combinators to be independent of Exp. */
trait ExpressionSynthesis extends SemanticTypes {

  // to be provided
  val history: History
 // val domain: DomainModel = history.flatten
  val allTests:UnitSuite

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
  @combinator object TestCases {
    def apply:CompilationUnit = {

      // each test creates a public void testXXX method, inside of which, each sub-part is evaluated.
      var testNumber = 0
      val allGen:String = allTests.iterator.asScala.map(tst => {

        // build up expression for which all tests are defined.
        // Note that we might need model in addition to test case itself to properly construct this
        // either:   Add add = new AddFinal(new LitFinal(7), new LitFinal(4));
        //           AddPrettyPCollectFinal addpc = new AddPrettyPCollectFinal(new LitPrettyPCollectFinal(3), new LitPrettyPCollectFinal(4));
        val testDomain = new TestCaseCodeGenerators(history, tst)
        val code:Option[com.github.javaparser.ast.expr.Expression] = testDomain.instanceGenerators(tst.expression)
        testNumber = testNumber+1
        var subTypes:String = testDomain.computeSubTypes()
        if (subTypes.length == 0) {
          subTypes = "Exp"   // need to have in case all subtypes vanish.
        }
        var init:String = s"""$subTypes exp$testNumber = ${code.get.toString};"""

        // each individual test case is evaluated within the context of this expression
        var resultNumber = 0
        val blocks:Seq[Statement] = tst.iterator().asScala.flatMap(tc => {

          resultNumber = resultNumber + 1

          // Convert the INSTANCE into proper instantiation code for the Visitor. A generator does this.
          val op:Operation = tc.op

          // hah! once again, I am stuck using a case statement, which will need to be expanded
          // into a code generator for maximum power
          val generated:Seq[Statement] = op match{
            case _:Eval =>
              Java(s"""|  assertEquals(${tc.expected.toString}, exp$testNumber.eval());
                       |""".stripMargin).statements()

            case _:PrettyP =>
              Java(s"""|  assertEquals("${tc.expected.toString}", exp$testNumber.print());
                       |""".stripMargin).statements()

            // unless I export the full domain model visitor pattern into the solution domain,
            // we must rely on the prettyP operation for our success. Make sure that PrettyP is available
            // TODO: Actually only need to have PrettyP and SimplifyExpr, which means not maximal set
            // TODO: but minimal set that includes these two...
            case _:SimplifyExpr => {
              //val subTypes:String = testDomain.computeSubTypesEnsure(List(new PrettyP))
              val tstDomain = new TestCaseCodeGenerators(history, tst, List(new PrettyP))
              val expectedCode: Option[com.github.javaparser.ast.expr.Expression] = tstDomain.instanceGenerators(tc.expected.asInstanceOf[Instance])
              subTypes = tstDomain.computeSubTypes()
              if (expectedCode.isDefined) {

                val newInitCode = new TestCaseCodeGenerators(history, tst, List(new PrettyP)).instanceGenerators(tst.expression) // needs PrettyP
                init = s"""$subTypes exp$testNumber = ${newInitCode.get.toString};"""

                val initExpected: String = s"""$subTypes expectedExp$testNumber = ${expectedCode.get.toString};"""

                val str:String = s"""|  $initExpected
                                     |  $subTypes result$resultNumber = ($subTypes) exp$testNumber.simplify();
                                     |  assertEquals(expectedExp$testNumber.print(), result$resultNumber.print());
                                     |""".stripMargin
                Java(str).statements()
              }
              else {
                // skip for lack of anything better.
                Java("// skip${op.name}$testNumber(){}\n").statements()
              }
            }

            case  c:Collect => {
              var expected:String = "java.util.List<Double> match = new java.util.ArrayList<Double>();"

              expected += tc.expected.asInstanceOf[java.util.List[Lit]].asScala.map(value => {
                "match.add(" + value.value + ");"
              }).mkString("\n")

              Java(s"""|  java.util.List<Double> result$resultNumber = (java.util.List<Double>) exp$testNumber.collectList();
                       |  $expected
                       |  assertEquals(match, result$resultNumber);
                       |""".stripMargin).statements()
            }

            case _ => Java(s"""// skip${op.name}$testNumber(){}""").statements()
          }

          generated
        }).toSeq

        val codeBlock:String = s"""|public void testEval$testNumber() {
                                   |  $init
                                   |  ${blocks.mkString("\n")}
                                   |}""".stripMargin
        Java(codeBlock).methodDeclarations().mkString("\n")
      }).mkString("\n")

      Java(s"""|package ep;
               |import junit.framework.TestCase;
               |public class TestSuite extends TestCase {
               |    $allGen
               |}""".stripMargin).compilationUnit()
    }

    val semanticType:Type = driver
  }
}