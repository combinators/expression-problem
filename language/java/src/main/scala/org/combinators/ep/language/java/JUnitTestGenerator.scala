package org.combinators.ep.language.java    /*DI:LD:AI*/

import org.combinators.ep.domain.{BaseDomain, Evolution}
import org.combinators.templating.twirl.Java

/**
  * Each evolution has opportunity to enhance the code generators.
  *
  * This code conforms to JUnit Test cases
  */
class JUnitTestGenerator(val evolution:Evolution) extends TestGenerator with PerformanceTestGenerator { // had  with JavaGenerator
  val domain: BaseDomain

  /** Combine all test cases together into a single JUnit 3.0 TestSuite class. */
  def generateSuite(pkg: Option[String]): Seq[CompilationUnit] = {
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

    files
  }
}