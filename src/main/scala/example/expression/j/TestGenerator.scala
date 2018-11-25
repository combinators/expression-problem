package example.expression.j   /*DI:LD:AI*/

import com.github.javaparser.ast.body.MethodDeclaration
import example.expression.domain.{BaseDomain, ModelDomain}
import org.combinators.templating.twirl.Java

trait TestGenerator extends JavaGenerator {
  val domain: BaseDomain with ModelDomain
  import domain._

  /** Return properly formatted expected value as a string. */
  def expected(test:TestCase, id:String) : (Expression => Seq[Statement]) => Seq[Statement] = continue => {
    continue(Java(test.expect._2.toString).expression[Expression])
  }

  /** Actual value in a test case. */
  def actual(test:domain.TestCase):Expression = dispatch(convert(test.inst), test.op)

  /** Convert a test instance into a Java Expression for instantiating that instance. */
  def convert(inst: AtomicInst): Expression = {
    val name = inst.e.name
    inst match {
      case ui: UnaryInst =>
        Java(s"new $name(${convert(ui.inner)})").expression()
      case bi: BinaryInst =>
        val left = convert(bi.left)
        val right = convert(bi.right)
        Java(s"new $name($left, $right)").expression()
      case exp: AtomicInst => Java(s"new $name(${exp.i.get})").expression()

      case _ => Java(s""" "unknown $name" """).expression()
    }
  }

  /** Return sample test cases as methods. */
  def testGenerator: Seq[MethodDeclaration] = Seq.empty

  /** Return MethodDeclaration associated with given test cases. */
  def testMethod(tests:Seq[TestCase]) : MethodDeclaration = {

    val stmts:Seq[Statement] = tests.zipWithIndex.flatMap(pair => {
      val test = pair._1
      val idx = pair._2

      val id:String = s"v$idx"

      // The expected method takes in a function that will be called by the expected method. Now, the expected
      // method will pass in the expression (which is expected) into this function, and it is the job of that
      // function to return the variable.
      expected(test, id)(expectedExpr => Java(s"assertEquals($expectedExpr, ${actual(test)});").statements)
    })

    Java(s"""|public void test() {
             |   ${stmts.mkString("\n")}
             |}""".stripMargin).methodDeclarations.head
  }
}
