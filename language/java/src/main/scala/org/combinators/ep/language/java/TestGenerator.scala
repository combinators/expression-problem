package org.combinators.ep.language.java      /*DI:LD:AI*/

import com.github.javaparser.ast.body.MethodDeclaration
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.generator.DomainIndependentTestGenerator
import org.combinators.templating.twirl.Java

abstract class TestGenerator (val gen:JavaGenerator) extends DomainIndependentTestGenerator(gen) {
  import langGen._

  type UnitTest = MethodDeclaration /** Base concept for the representation of a single test case. */

  /** Converts types in test code. */
//  def testTypeConverter(ty: TypeRep) : Type = {
//    tpe(ty)
//  }

  /**
    * Represents the sequence of total test cases.
    */
  def testGenerator : Seq[UnitTest] = Seq.empty

  /** Return MethodDeclaration associated with given test cases. */
  @throws[NotImplementedError]("if unknown TestCase gets here.")
  def junitTestMethod(test: TestCase, idx: Int): Seq[Statement] = {

     test match {
       case eq: EqualsTestCase =>
         val expectedBlock = instantiate(eq.expected)
         val parameterBlock =
           eq.params.foldLeft(CodeBlockWithResultingExpressions.empty) {
             case (b, p) => b.appendIndependent(instantiate(p))
           }

         val actualBlock = parameterBlock.appendDependent(params =>
             actual(eq.op, eq.expected, params.asInstanceOf: _*).asInstanceOf    // was eq.inst
         )

         expectedBlock.appendDependent { case Seq(expectedValue) =>
           actualBlock.appendDependent { case Seq(actualValue) =>
             CodeBlockWithResultingExpressions(Java(s"assertEquals($expectedValue, $actualValue);").statement().asInstanceOf[Statement])()
           }
         }.block

       case ne: NotEqualsTestCase =>
         val unExpectedBlock = instantiate(ne.expected)
         val parameterBlock =
           ne.params.foldLeft(CodeBlockWithResultingExpressions.empty) {
             case (b, p) => b.appendIndependent(instantiate(p))
           }
         val actualBlock =
           parameterBlock.appendDependent(params =>
             actual(ne.op, ne.expected, params.asInstanceOf: _*).asInstanceOf    // was ne.inst
           )

         unExpectedBlock.appendDependent { case Seq(unExpectedValue) =>
           actualBlock.appendDependent { case Seq(actualValue) =>
             CodeBlockWithResultingExpressions(Java(s"assertNotEquals($unExpectedValue, $actualValue);").statement().asInstanceOf[Statement])()
           }
         }.block

       case seq: EqualsCompositeTestCase =>
         val expectedBlock = instantiate(seq.expected)
         val actualStartBlock = {
           val parameterBlock =
             seq.ops.head._2.foldLeft(CodeBlockWithResultingExpressions.empty) {
               case (b, p) => b.appendIndependent(instantiate(p))
             }
           parameterBlock.appendDependent(params =>
             actual(seq.ops.head._1, seq.expected, params.asInstanceOf: _*).asInstanceOf      // had been seq.inst
           )
         }
         val actualBlock = seq.ops.tail.foldLeft(actualStartBlock) { case (currentBlock, (nextOp, nextParams)) =>
           currentBlock.appendDependent { case Seq(currentResult) =>
             val parameterBlock =
               nextParams.foldLeft(CodeBlockWithResultingExpressions.empty) {
                 case (b, p) => b.appendIndependent(instantiate(p))
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
             CodeBlockWithResultingExpressions(Java(s"assertEquals($expectedValue, $actualValue); ").statement().asInstanceOf[Statement])()
           }
         }.block

         // resolve all else with error
       case _ =>
         throw new scala.NotImplementedError(s"""No rule to generate test case for "$test".""")
     }
  }

  /** Return MethodDeclaration associated with given test cases. */
  def testMethod(tests: Seq[TestCase]): Seq[UnitTest] = {
    val stmts = tests.zipWithIndex.flatMap { case (test, idx) => junitTestMethod(test, idx) }
    if (stmts.isEmpty) {
      Seq.empty
    } else {
      Java(
        s"""|public void test() {
            |   ${stmts.mkString("\n")}
            |}""".stripMargin).methodDeclarations
    }
  }
}
