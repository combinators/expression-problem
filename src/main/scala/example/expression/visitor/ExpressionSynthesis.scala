package example.expression.visitor

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.stmt.Statement
import org.combinators.cls.interpreter.combinator
import org.combinators.cls.types.Type
import org.combinators.cls.types.syntax._
import org.combinators.templating.twirl.Java
import expression._
import expression.data.Eval
import expression.extensions._
import expression.history.History
import expression.instances.{Instance, Lit, UnitSuite}
import expression.operations.SimplifyExpr
import shared.compilation.CodeGeneratorRegistry

import scala.collection.JavaConverters._

/** Future work: reduce the need to have full set of traits here, with the internals accessing the code generators. Separate concerns */
trait ExpressionSynthesis extends InstanceCodeGenerators with SemanticTypes {

  // to be provided
  val history: History
  val domain: DomainModel = history.flatten
  val allTests:UnitSuite

  /**
    * Combinator to identify the constraint generator to use when generating instances
    */
  @combinator object InstanceGenerator {
    def apply: CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression] = defaultInstance.instanceGenerators

    val semanticType: Type = generator(generator.instance)
  }

  /** Construct visitor abstract class. */
  @combinator object Visitor {
    def apply(): CompilationUnit = {
      val signatures = domain.data.asScala
        .map(x => s"public abstract R visit(${x.getClass.getSimpleName} exp);").mkString("\n")

      Java (s"""
               |package expression;
               |/*
               | * A concrete visitor describes a concrete operation on expressions. There is one visit
               | * method per type in the class hierarchy.
               | */
               |public abstract class Visitor<R> {
               |
               |$signatures
               |}
         """.stripMargin).compilationUnit()
    }

    val semanticType:Type = generated(generated.visitor)
  }

  /** Generate from domain. USER NEEDS TO SPECIFY THESE EITHER AUTOMATICALLY OR MANUALLY */
  @combinator object BaseExpClass {
    def apply() : CompilationUnit =
      Java(s"""|package expression;
               |
               |public abstract class Exp {
               |    public abstract <R> R accept(Visitor<R> v);
               |}
               |""".stripMargin).compilationUnit()

    val semanticType:Type = exp(exp.base, new Exp)
  }

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
  @combinator object Driver {
    def apply(generator:CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression]):CompilationUnit = {

      // each test creates a public void testXXX method, inside of which, each sub-part is evaluated.
      var testNumber = 0
      val allGen:String = allTests.iterator.asScala.map(tst => {
        // build up expression for which all tests are defined.
        val code:Option[com.github.javaparser.ast.expr.Expression] = generator(tst.expression)
        testNumber = testNumber+1
        val init:String = s"""Exp exp$testNumber = ${code.get.toString};"""

        // each individual test case is evaluated within the context of this expression
        var resultNumber = 0
        val blocks:Seq[Statement] = tst.iterator().asScala.flatMap(tc => {

          //  val comb:Seq[Statement] = representationCodeGenerators.evalGenerators(tc).get

          resultNumber = resultNumber + 1
          // Convert the INSTANCE into proper instantiation code for the Visitor. A generator does this.
          val op:Operation = tc.op

          // hah! once again, I am stuck using a case statement, which will need to be expanded
          // into a code generator for maximum power
          val generated:Seq[Statement] = op match{
            case _:Eval =>
              Java(s"""|  Double result$resultNumber = (Double) exp$testNumber.accept(new Eval());
                       |  assertEquals(${tc.expected.toString}, result$resultNumber.doubleValue());
                       |""".stripMargin).statements()

            case _:PrettyP =>
              Java(s"""|  String result$resultNumber = (String) exp$testNumber.accept(new PrettyP());
                       |  assertEquals("${tc.expected.toString}", result$resultNumber);
                       |""".stripMargin).statements()

            // unless I export the full domain model visitor pattern into the solution domain,
            // we must rely on the prettyP operation for our success.
            case _:SimplifyExpr => {

              val expectedCode: Option[com.github.javaparser.ast.expr.Expression] = generator(tc.expected.asInstanceOf[Instance])
              if (expectedCode.isDefined) {
                val initExpected: String = s"""Exp expectedExp$testNumber = ${expectedCode.get.toString};"""

                // TODO: Create an equal visitor for use instead of depending on prettyP
                val str: String =
                  s"""
                     |  $initExpected
                     |  Exp result$resultNumber = (Exp) exp$testNumber.accept(new SimplifyExpr());
                     |  assertEquals(expectedExp$testNumber.accept(new PrettyP()), result$resultNumber.accept(new PrettyP()));
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

              Java(s"""|  java.util.List<Double> result$resultNumber = (java.util.List<Double>) exp$testNumber.accept(new Collect());
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

      Java(s"""|package expression;
               |import junit.framework.TestCase;
               |public class TestSuite extends TestCase {
               |    $allGen
               |}""".stripMargin).compilationUnit()
    }

    val semanticType:Type = generator(generator.instance) =>: driver
  }

}
