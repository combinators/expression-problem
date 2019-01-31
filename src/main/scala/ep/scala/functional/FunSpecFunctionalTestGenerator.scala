package ep.scala.functional   /*DI:LD:AD*/

import java.nio.file.Paths

import ep.domain.{BaseDomain, ModelDomain}
import ep.scala.{FunSpecTestGenerator, Scala, ScalaTestWithPath, ScalaWithPath}

trait FunSpecFunctionalTestGenerator extends FunSpecTestGenerator {
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
    val withClause = model.get.inChronologicalOrder.map(m => s"with ${m.name.capitalize}").mkString(" ")
    val allTests = testGenerator ++ performanceMethod

    val helpers:Seq[String] = model.get.flatten().ops.map(op => {
      if (op.parameters.isEmpty) {
        s"  override def ${op.name.toLowerCase}:visitor with ${op.name.capitalize} = new Visitor with ${op.name.capitalize}"
      } else {
        val paramsDef = op.parameters.map(param => s"_${param.name}: ${typeConverter(param.tpe)}").mkString(",")
        val paramsSet = op.parameters.map(param => s"val ${param.name} = _${param.name}").mkString("\n")
        s"  override def ${op.name.toLowerCase}($paramsDef):visitor with ${op.name.capitalize} = new Visitor with ${op.name.capitalize} { $paramsSet }"
      }
    })

    val files:Seq[ScalaWithPath] = allTests.zipWithIndex.map{ case (md, num) =>
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
           |      test()
           |    }
           |
           |    ${md.mkString("\n")}
           |  }
           |}""".stripMargin).source(), Paths.get(s"TestSuite$num.scala"))
    }

    files
  }
  
}
