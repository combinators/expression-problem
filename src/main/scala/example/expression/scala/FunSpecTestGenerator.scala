package example.expression.scala    /*DI:LD:AI*/

import java.nio.file.Paths

import example.expression.domain.{BaseDomain, ModelDomain}

import scala.meta._

/**
  * Each evolution has opportunity to enhance the code generators.
  *
  * This code conforms to JUnit Test cases
  */
trait FunSpecTestGenerator extends TestGenerator with ScalaGenerator {
  val domain: BaseDomain with ModelDomain
  import domain._

  /** Type to use when referring to specific instance. */
  def exprDefine(exp:AtomicInst) : Type = {
    Scala(exp.e.name).tpe()
  }

  /** Used when one already has code fragments bound to variables, which are to be used for left and right. */
  def convertRecursive(inst: Binary, left:String, right:String): Expression = {
    val name = inst.name
    Scala(s"new $name($left, $right)").term()
  }

  /** Combine all test cases together into a single JUnit 3.0 TestSuite class. */
  def generateSuite(pkg: Option[String], model: Option[Model] = None): Seq[ScalaWithPath] = {
    val packageDeclaration: String = if (pkg.isDefined) {
      s"package ${pkg.get}"
    } else {
      ""
    }

    val allTests = testGenerator

    var num: Int = 0
    val files: Seq[ScalaWithPath] = allTests.map(md => {
      num = num + 1

      ScalaWithPath(Scala(s"""
               |$packageDeclaration
               |import org.scalatest.FunSpec
               |
               |class TestSuite$num extends FunSpec  {
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