package org.combinators.ep.language.scala.oo   /*DI:LD:AD*/

import java.nio.file.Paths

import org.combinators.ep.language.scala.{FunSpecTestGenerator, Scala, ScalaTestWithPath, ScalaWithPath}
import org.combinators.ep.domain.{BaseDomain, ModelDomain}

// Note: never got these test cases to work...

trait FunSpecOOTestGenerator extends FunSpecTestGenerator {
  val domain: BaseDomain with ModelDomain

  /**
    * Combine all test cases together into a single JUnit 3.0 TestSuite class.
    *
    * Annoying to override entire method JUST to add the "with ..." clause. Perhaps this
    * could be revised later.
    */
  override def generateSuite(pkg: Option[String]): Seq[ScalaWithPath] = {
    val model = getModel
    val packageDeclaration: String = if (pkg.isDefined) {
      s"package ${pkg.get}"
    } else {
      ""
    }

    // t is a Seq[Stat] so we have to expand with mkString. Would inject ${t.mkString("\n")} below
    testGenerator.zipWithIndex.map{ case (t, num) =>
     ScalaTestWithPath(Scala(s"""
                             |$packageDeclaration
                             |import org.scalatest.funspec.AnyFunSpec
                             |
                             |class TestSuite$num extends AnyFunSpec with ${model.name.capitalize} {
                             |  describe("test cases") {
                             |    it ("run test") {
                             |      alert("Not generating test cases for Odersky")
                             |      assert(false == true)
                             |    }
                             |  }
                             |}""".stripMargin).source(), Paths.get(s"TestSuite$num.scala"))
    }
  }
}
