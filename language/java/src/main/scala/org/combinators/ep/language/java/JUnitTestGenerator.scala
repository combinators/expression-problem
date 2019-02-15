package org.combinators.ep.language.java    /*DI:LD:AI*/

import org.combinators.ep.domain.{BaseDomain, ModelDomain}
import org.combinators.templating.twirl.Java

/**
  * Each evolution has opportunity to enhance the code generators.
  *
  * This code conforms to JUnit Test cases
  */
trait JUnitTestGenerator extends TestGenerator with PerformanceTestGenerator with JavaGenerator {
  val domain: BaseDomain with ModelDomain
  import domain._



  /** Combine all test cases together into a single JUnit 3.0 TestSuite class. */
  def generateSuite(pkg: Option[String], model: Option[Model] = None): Seq[CompilationUnit] = {
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