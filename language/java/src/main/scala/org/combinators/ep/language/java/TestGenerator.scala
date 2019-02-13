package org.combinators.ep.language.java

/*DI:LD:AI*/

import com.github.javaparser.ast.body.MethodDeclaration
import org.combinators.ep.domain.{BaseDomain, ModelDomain}
import org.combinators.ep.generator.LanguageIndependentTestGenerator
import org.combinators.templating.twirl.Java

trait TestGenerator extends JavaGenerator with LanguageIndependentTestGenerator {
  val domain: BaseDomain with ModelDomain
  import domain._

  type UnitTest = MethodDeclaration /** Base concept for the representation of a single test case. */

  /** Return sample test cases as methods. */
  def testGenerator: Seq[MethodDeclaration] = Seq.empty

  /** Return MethodDeclaration associated with given test cases. */
  def junitTestMethod(test: TestCase, idx: Int): Seq[Statement] = {
     test match {
       case eq: EqualsTestCase =>
         val expectedBlock = toTargetLanguage(eq.expect)
         val parameterBlock =
           eq.params.foldLeft(CodeBlockWithResultingExpressions.empty) {
             case (b, p) => b.appendIndependent(toTargetLanguage(p))
           }
         val actualBlock =
           parameterBlock.appendDependent(params =>
             actual(eq.op, eq.inst, params: _*)
           )

         expectedBlock.appendDependent(expectedValue =>
           actualBlock.appendDependent(actualValue =>
             CodeBlockWithResultingExpressions(Java(s"assertEquals($expectedValue, $actualValue);").statement())()
           )
         ).block

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

         unExpectedBlock.appendDependent(unExpectedValue =>
           actualBlock.appendDependent(actualValue =>
             CodeBlockWithResultingExpressions(Java(s"assertNotEquals($unExpectedValue, $actualValue);").statement())()
           )
         ).block
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

         expectedBlock.appendDependent(expectedValue =>
           actualBlock.appendDependent(actualValue =>
             CodeBlockWithResultingExpressions(Java(s"assertEquals($expectedValue, $actualValue);").statement())()
           )
         ).block
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
