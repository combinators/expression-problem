package ep.scala    /*DI:LD:AI*/

import java.nio.file.Paths

import ep.domain.{BaseDomain, ModelDomain}

import scala.meta._

/**
  * Each evolution has opportunity to enhance the code generators.
  *
  * This code conforms to JUnit Test cases
  */
trait FunSpecTestGenerator extends TestGenerator with ScalaGenerator {
  val domain: BaseDomain with ModelDomain
  import domain._

  /** Combine all test cases together into a single JUnit 3.0 TestSuite class. */
  def generateSuite(pkg: Option[String], model: Option[Model] = None): Seq[ScalaWithPath] = {
    val packageDeclaration: String = if (pkg.isDefined) {
      s"package ${pkg.get}"
    } else {
      ""
    }

    val allTests = testGenerator ++ performanceMethod
    val files: Seq[ScalaWithPath] = allTests.zipWithIndex.map{ case (t, num) =>
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

    files
  }
}