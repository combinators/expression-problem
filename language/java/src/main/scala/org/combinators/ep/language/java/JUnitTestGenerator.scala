package org.combinators.ep.language.java    /*DI:LD:AI*/

import com.github.javaparser.ast.body.MethodDeclaration
import org.combinators.ep.domain.abstractions.TestCase
import org.combinators.templating.twirl.Java

/**
  * Each evolution has opportunity to enhance the code generators.
  *
  * This code conforms to JUnit Test cases.
  *
  * For now skip PerformanceGenerators....
  */
case class JUnitTestGenerator() extends TestGenerator(JavaGenerator()) {
  import langGen.CompilationUnit

  /** Combine all test cases together into a single JUnit 3.0 TestSuite class. */
  override def generateSuite(pkg: Option[String]): Seq[CompilationUnit] = {
    val packageDeclaration: String = if (pkg.isDefined) {
      s"package ${pkg.get};"
    } else {
      ""
    }

    val allTests = testGenerator
    val files = allTests.filter(md => md.getBody.isPresent).zipWithIndex.map(pair => {
      Java(s"""|$packageDeclaration
               |import junit.framework.TestCase;
               |public class TestSuite${pair._2} extends TestCase {
               |    ${pair._1}
               |}""".stripMargin).compilationUnit
    })

    files.asInstanceOf
  }

  /**
   * Traits can override this method to add their test cases to the mix.
   *
   * Common usage is:
   *
   * {{{
   *   abstract override def testGenerator: Seq[UnitTest] = {
   *     super.testGenerator ++ testMethod(M4_tests)
   *   }
   * }}}
   *
   * @param tests sequence of candidate test cases
   * @return Code fragments (based on language and approach) representing unit test cases.
   */
  override def testMethods(tests: Seq[TestCase]): Seq[MethodDeclaration] = Seq.empty
}
