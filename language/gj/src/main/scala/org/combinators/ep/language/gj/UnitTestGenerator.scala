package org.combinators.ep.language.gj   /*DI:LD:AI*/

import org.combinators.ep.domain.{BaseDomain, ModelDomain}
import java.nio.file.Paths

/**
  * Each evolution has opportunity to enhance the code generators.
  *
  * If there were a JUnit integrated with GJ then that would be useful!
  */
trait UnitTestGenerator extends TestGenerator with PerformanceTestGenerator with GJGenerator {
  val domain: BaseDomain with ModelDomain

  /** Combine all test cases together into a single class. */
  def generateSuite(pkg: Option[String]): Seq[CompilationUnit] = {
    val packageDeclaration: String = if (pkg.isDefined) {
      s"package ${pkg.get};"
    } else {
      ""
    }

    val allTests = testGenerator
    val files = allTests.zipWithIndex.map(pair => {
      GJWithPath(GJ(s"""|$packageDeclaration
               |public final class TestSuite${pair._2} {
               |    ${pair._1.mkString("\n")}
               |}""".stripMargin), Paths.get(s"TestSuite${pair._2}.gj"))
    })

    files
  }
}