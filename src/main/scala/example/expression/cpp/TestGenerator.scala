package example.expression.cpp

/*DI:LD:AI*/

import example.expression.domain.{BaseDomain, ModelDomain}
import org.combinators.templating.twirl.Java

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait TestGenerator {
  val domain: BaseDomain with ModelDomain

  import domain._

  /** Return sample JUnit test cases. */
  def testGenerator: Seq[CPPElement] = Seq.empty

  /** Convert a test instance into a Java Expression for instantiating that instance. */
  def convert(inst: AtomicInst): CPPElement = {
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

  /** Combine all test cases together into a single JUnit 3.0 TestSuite class. */
  def generateSuite(pkg: Option[String], model: Option[Model] = None): Seq[CPPFile] = {
    val packageDeclaration: String = if (pkg.isDefined) {
      s"package ${pkg.get};"
    } else {
      ""
    }

    var num: Int = 0
    val files: Seq[CPPFile] = testGenerator.map(md => {
      num = num + 1
      new CPPClass(s"TestSuite$num", "", Seq(md), Seq.empty)
    })

    //Java (s"public void test$num() ${md.getBody.get}").methodDeclarations
    files
  }
}