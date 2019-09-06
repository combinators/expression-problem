package org.combinators.ep.language.scala     /*DI:LD:AI*/

import java.nio.file.Paths

import org.combinators.ep.domain.BaseDomain

/**
  * Each evolution has opportunity to enhance the code generators.
  *
  * This code conforms to JUnit Test cases
  */
trait FunSpecTestGenerator extends TestGenerator with ScalaGenerator with PerformanceTestGenerator {
  val domain: BaseDomain with ModelDomain

  /**
    * Represents the sequence of total test cases.
    */
  def testGenerator : Seq[UnitTest] = Seq.empty

  /** Combine all test cases together into a single JUnit 3.0 TestSuite class. */
  def generateSuite(pkg: Option[String]): Seq[ScalaWithPath] = {
    val packageDeclaration: String = if (pkg.isDefined) {
      s"package ${pkg.get}"
    } else {
      ""
    }

    // t is a Seq[Stat] so we have to expand with mkString
    testGenerator.zipWithIndex.map{ case (t, num) => {
      ScalaTestWithPath(Scala(s"""
               |$packageDeclaration
               |import org.scalatest.FunSpec
               |
               |class TestSuite$num extends FunSpec  {
               |  describe("test cases") {
               |    it ("run test") {
               |      ${t.mkString("\n")}
               |    }
               |  }
               |}""".stripMargin).source(), Paths.get(s"TestSuite$num.scala"))

    }
  }}
}