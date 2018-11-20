package example.expression.scala

import example.expression.domain.{BaseDomain, ModelDomain}

import scala.meta.{Stat, Term}

trait TestGenerator extends ScalaGenerator {
  val domain: BaseDomain with ModelDomain
  import domain._

  /** Return properly formatted expected value as a string. */
  def expected(test:TestCase, id:String) : (Term => Stat) => Stat = continue => {
    continue(Scala(test.expect._2.toString).term())
  }

  /** Actual value in a test case. */
  def actual(test:domain.TestCase):Expression = dispatch(convert(test.inst), test.op)

  /** Convert a test instance into a Java Expression for instantiating that instance. */
  def convert(inst: AtomicInst): Expression = {
    val name = inst.e.name
    inst match {
      case ui: UnaryInst =>
        Scala(s"new $name(${convert(ui.inner)})").expression()
      case bi: BinaryInst =>
        val left = convert(bi.left)
        val right = convert(bi.right)
        Scala(s"new $name($left, $right)").expression()
      case exp: AtomicInst => Scala(s"new $name(${exp.i.get})").expression()

      case _ => Scala(s""" "unknown $name" """).expression()
    }
  }

  /** Return sample test cases as methods. */
  def testGenerator: Seq[Stat] = Seq.empty

  /** Return MethodDeclaration associated with given test cases. */
  def testMethod(tests:Seq[TestCase]) : Stat = {

    val stmts:Seq[scala.meta.Stat] = tests.zipWithIndex.map(pair => {
      val test = pair._1
      val idx = pair._2

      val id:String = s"v$idx"

      // The expected method takes in a function that will be called by the expected method. Now, the expected
      // method will pass in the expression (which is expected) into this function, and it is the job of that
      // function to return the variable.
      expected(test, id)(expectedExpr => Scala(s"assert ($expectedExpr == ${actual(test)})").statement())
    })

    Scala(s"""
            |def test() : Unit = {
            |   ${stmts.mkString("\n")}
            |}""".stripMargin).declaration()
  }
}
