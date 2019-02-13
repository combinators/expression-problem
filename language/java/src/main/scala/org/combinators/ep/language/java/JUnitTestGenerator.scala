package ep.j  /*DI:LD:AI*/

import ep.domain.ModelDomain
import org.combinators.ep.domain.{BaseDomain, ModelDomain}
import org.combinators.templating.twirl.Java

/**
  * Each evolution has opportunity to enhance the code generators.
  *
  * This code conforms to JUnit Test cases
  */
trait JUnitTestGenerator extends TestGenerator with JavaGenerator {
  val domain: BaseDomain with ModelDomain
  import domain._

  /** Type to use when referring to specific instance. */
  def exprDefine(exp:Inst) : Type = {
    Java(exp.name).tpe()
  }

  /** Used when one already has code fragments bound to variables, which are to be used for left and right. */
  def convertRecursive(inst: Binary, left:String, right:String): Expression = {
    val name = inst.name
    Java(s"new $name($left, $right)").expression()
  }

  /** Combine all test cases together into a single JUnit 3.0 TestSuite class. */
  def generateSuite(pkg: Option[String], model: Option[Model] = None): Seq[CompilationUnit] = {
    val packageDeclaration: String = if (pkg.isDefined) {
      s"package ${pkg.get};"
    } else {
      ""
    }

    val allTests = testGenerator ++ performanceMethod()

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