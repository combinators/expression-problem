package ep.scala.oo   /*DI:LD:AD*/

import java.nio.file.Paths

import ep.domain.{BaseDomain, ModelDomain}
import ep.scala.{FunSpecTestGenerator, Scala, ScalaTestWithPath, ScalaWithPath}

trait FunSpecOOTestGenerator extends FunSpecTestGenerator {
  val domain: BaseDomain with ModelDomain
  import domain._

  /**
    * Combine all test cases together into a single JUnit 3.0 TestSuite class.
    *
    * Annoying to override entire method JUST to add the "with ..." clause. Perhaps this
    * could be revised later.
    */
  override def generateSuite(pkg: Option[String], m: Option[Model] = None): Seq[ScalaWithPath] = {
    val packageDeclaration: String = if (pkg.isDefined) {
      s"package ${pkg.get}"
    } else {
      ""
    }

    val model = Some(getModel)
    val allTests = testGenerator ++ performanceMethod
    val files: Seq[ScalaWithPath] = allTests.zipWithIndex.map{ case (t, num) =>
     ScalaTestWithPath(Scala(s"""
                             |$packageDeclaration
                             |import org.scalatest.FunSpec
                             |
                             |class TestSuite$num extends FunSpec with ${model.get.name.capitalize} {
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
