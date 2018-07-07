package example.expression.j

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.expr.Expression
import example.expression.domain.{BaseDomain, ModelDomain}
import org.combinators.templating.twirl.Java

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait TestGenerator  {
  val domain:BaseDomain with ModelDomain

  /** Return sample JUnit test cases. */
  def testGenerator: Seq[MethodDeclaration] = Seq.empty

  /** Convert a test instance into a Java Expression for instantiating that instance. */
  def convert(inst:domain.AtomicInst) : Expression = {
    val name = inst.e.name
    inst match {
      case ui:domain.UnaryInst =>
        Java(s"new $name(${convert(ui.inner)})").expression()
      case bi:domain.BinaryInst =>
        Java(s"new $name(${convert(bi.left)}, ${convert(bi.right)})").expression()
      case exp:domain.AtomicInst => Java(s"new $name(${exp.i.get.toString})").expression()

      case _ =>  Java(s""" "unknown $name" """).expression()
    }
  }

  /** Combine all test cases together into a single JUnit 3.0 TestSuite class. */
  def generateSuite(pack:Option[String]): CompilationUnit = {
    val methods:Seq[MethodDeclaration] = testGenerator

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
