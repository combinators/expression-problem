package ep.scala.functional   /*DI:LD:AD*/

import java.nio.file.Paths

import ep.domain.{BaseDomain, ModelDomain}
import ep.scala.{FunSpecTestGenerator, Scala, ScalaTestWithPath, ScalaWithPath}

trait FunSpecFunctionalTestGenerator extends FunSpecTestGenerator {
  val domain: BaseDomain with ModelDomain
  import domain._


  // should be able to use scala meta transformations, since only adding with clauses
  /** Combine all test cases together into a single JUnit 3.0 TestSuite class. */
  override def generateSuite(pkg: Option[String], m: Option[Model] = None): Seq[ScalaWithPath] = {
    val packageDeclaration: String = if (pkg.isDefined) {
      s"package ${pkg.get}"
    } else {
      ""
    }

    val model = Some(getModel)
    val withClause = model.get.inChronologicalOrder.map(m => s"with ${m.name.capitalize}").mkString(" ")
    val allTests = testGenerator ++ performanceMethod

    val helpers:Seq[String] = model.get.flatten().ops.map(op => {
      if (op.parameters.isEmpty) {
        s"  override def ${op.instance}:visitor with ${op.concept} = new Visitor with ${op.concept}"
      } else {
        val paramsDef = op.parameters.map(param => s"_${param.name}: ${typeConverter(param.tpe)}").mkString(",")
        val paramsSet = op.parameters.map(param => s"val ${param.name} = _${param.name}").mkString("\n")
        s"  override def ${op.instance}($paramsDef):visitor with ${op.concept} = new Visitor with ${op.concept} { $paramsSet }"
      }
    })

    val files:Seq[ScalaWithPath] = allTests.zipWithIndex.map{ case (t, num) =>
      ScalaTestWithPath(Scala(s"""
           |$packageDeclaration
           |import org.scalatest.FunSpec
           |
           |class TestSuite$num extends FunSpec $withClause {
           |
           |  type visitor = Visitor
           |  ${helpers.mkString("\n")}
           |
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
