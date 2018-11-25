package example.expression.scala.oo

import java.nio.file.Paths

import example.expression.domain.{BaseDomain, ModelDomain}
import example.expression.scala.{FunSpecTestGenerator, Scala, ScalaWithPath}

trait FunSpecOOTestGenerator extends FunSpecTestGenerator {
  val domain: BaseDomain with ModelDomain
  import domain._


  // should be able to use scala meta transformations, since only adding with clauses
  /** Combine all test cases together into a single JUnit 3.0 TestSuite class. */
  override def generateSuite(pkg: Option[String], model: Option[Model] = None): Seq[ScalaWithPath] = {
    val packageDeclaration: String = if (pkg.isDefined) {
      s"package ${pkg.get}"
    } else {
      ""
    }

    // need to bring in last trait
    val withClause = if (model.isEmpty) {
      ""
    } else {
      println ("SAME:" + model.get.name + "," + model.get.base().name)
      if (model.get.equals(model.get.base())) {
        ""
      } else {
        s" with ${model.get.name.capitalize}"
      }
    }
    val allTests = testGenerator

    var num: Int = 0
    val files: Seq[ScalaWithPath] = allTests.map(md => {
      num = num + 1

      ScalaWithPath(Scala(s"""
                             |$packageDeclaration
                             |import org.scalatest.FunSpec
                             |
                             |class TestSuite$num extends FunSpec with Base $withClause {
                             |  describe("test cases") {
                             |    it ("run test") {
                             |      test()
                             |    }
                             |
                             |    $md
                             |  }
                             |}""".stripMargin).source(), Paths.get(s"TestSuite$num.scala"))

    })

    files
  }
  
}
