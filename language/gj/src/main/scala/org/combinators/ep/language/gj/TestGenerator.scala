package org.combinators.ep.language.gj      /*DI:LD:AI*/

import java.nio.file.Paths

import org.combinators.ep.domain.{BaseDomain, ModelDomain}

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait TestGenerator {
  val domain: BaseDomain with ModelDomain
  import domain._

  /** Return sample JUnit test cases. */
  def testGenerator: Seq[GJ] = Seq.empty

  /**
    * In test cases, languages are instantiated from which new is called.
    */
  def testDispatch(expr:GJ, op:domain.Operation, params:GJ*) : GJ = {
    val args:String = params.mkString(",")
    GJ(s"""$expr.visit(l.new ${op.concept}())""")
  }

  /** Convert a test instance into a GJ Expression for instantiating that instance. */
  def convert(inst: Inst): GJ = {
    val name = inst.name
    inst match {
      case ui: UnaryInst =>
        GJ(s"l.new $name(${convert(ui.inner)})")
      case bi: BinaryInst =>
        val left = convert(bi.left)
        val right = convert(bi.right)
        GJ(s"l.new $name($left, $right)")
      case exp: AtomicInst => GJ(s"l.new $name(${exp.ei.inst})")

      case _ => GJ(s""" "unknown $name" """)
    }
  }

  /** Combine all test cases together into a single JUnit 3.0 TestSuite class. */
  def generateSuite(pkg: Option[String], model: Option[Model] = None): Seq[GJWithPath] = {
    var num: Int = 0

    val methods = testGenerator.map(md => {
      num = num + 1
      GJ(s"""|
            |static public void test$num() {
            | $md
            |}""".stripMargin).getCode
      }).mkString("\n")

    val invocations = (1 to num).map(d => s"test$d();").mkString("\n")
    val code = GJ(
      s"""
         |final class TestSuite {
         |  $methods
         |  static public void main (String[] args) {
         |    $invocations
         |  }
         |}
       """.stripMargin)

    Seq(GJWithPath(code, Paths.get(s"TestSuite.gj")))
  }
}