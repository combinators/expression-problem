package example.expression.j

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.expr.Expression
import example.expression.domain.Domain
import org.combinators.templating.twirl.Java

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait TestGenerator {
  val domain:Domain
  import domain._

  /** Return sample JUnit test cases. */
  def testGenerator(model:Model): Seq[MethodDeclaration] = Seq.empty

  /** Convert a test instance into a Java Expression for instantiating that instance. */
  def convert(inst:instances.ExpInst, model:Model) : Expression = {
    val name = inst.e.name
    inst match {
      case lit:LitInst => Java(s"new $name(${lit.i.get.toString})").expression()
      case ui:instances.UnaryExpInst =>
        Java(s"new $name(${convert(ui.exp, model)})").expression()
      case bi:instances.BinaryExpInst =>
        Java(s"new $name(${convert(bi.left, model)}, ${convert(bi.right, model)})").expression()

      case _ =>  Java(s""" "unknown $name" """).expression()
    }
  }

  /** Combine all test cases together into a single JUnit 3.0 TestSuite class. */
  def generateSuite(pack:Option[String], model:Model): CompilationUnit = {
    val methods:Seq[MethodDeclaration] = testGenerator(model)

    val packageDeclaration:String = if (pack.isDefined) {
      s"package ${pack.get};"
    } else { "" }

    var num:Int = 0
    val unitTests:Seq[MethodDeclaration] = methods.filter(md => md.getBody.isPresent).map(md => {
      num = num + 1
      Java (s"""public void test$num()  ${md.getBody.get.toString} """).methodDeclarations().head
    })

    Java(s"""|$packageDeclaration
             |import junit.framework.TestCase;
             |public class TestSuite extends TestCase {
             |    ${unitTests.mkString("\n")}
             |}""".stripMargin).compilationUnit()
  }

}







