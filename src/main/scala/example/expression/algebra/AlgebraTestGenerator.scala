package example.expression.algebra

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.body.{FieldDeclaration, MethodDeclaration}
import com.github.javaparser.ast.expr.Expression
import com.github.javaparser.ast.stmt.Statement
import example.expression.domain.Domain
import example.expression.j.TestGenerator
import org.combinators.templating.twirl.Java

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait AlgebraTestGenerator extends TestGenerator {
  val domain: Domain
  import domain._

  // exists from AbstractGenerator
  def typeGenerator(tpe:types.Types) : com.github.javaparser.ast.`type`.Type

  /** Convert a test instance into a Java Expression for instantiating that instance. */
  override def convert(inst: instances.ExpInst, model: Model): Expression = {
    val name = inst.e.name
    val opname = name.toLowerCase()
    inst match {
      case lit: LitInst => Java(s"algebra.lit(${lit.i.get.toString})").expression()
      case ui: instances.UnaryExpInst =>
        Java(s"algebra.$opname(${convert(ui.exp, model)})").expression()
      case bi: instances.BinaryExpInst =>
        Java(s"algebra.$opname(${convert(bi.left, model)}, ${convert(bi.right, model)})").expression()

      case _ => Java(s""" "unknown $name" """).expression()
    }
  }

  /**
    * Classify model based on current or most recently defined types.
    *
    * The classification is a sorted concatenation of the most recent model (including self) that
    * defines new data types.
    */
  def classify(m:Model) : String = {
    if (m.isEmpty) {
      return ""
    }
    if (m.types.isEmpty) {
      return classify(m.last)
    }

    m.types.sortWith(_.name < _.name).map(op => op.name.capitalize).mkString("")
  }

  /** Combine all test cases together into a single JUnit 3.0 TestSuite class. */
  override def generateSuite(pack: Option[String], m: Model): CompilationUnit = {
    val methods: Seq[MethodDeclaration] = testGenerator(m)

    val packageDeclaration: String = if (pack.isDefined) {
      s"package ${pack.get};"
    } else {
      ""
    }

    var num: Int = 0
    val unitTests: Seq[MethodDeclaration] = methods.filter(md => md.getBody.isPresent).map(md => {
      num = num + 1
      Java(s"public void test$num() ${md.getBody.get.toString}").methodDeclarations().head
    })

    // must get all operations defined for this model and earlier. For each one, define algebra with
    // current extension
    val operations: Seq[Operation] = m.flat().ops
    var algebraDeclarations: Map[Operation, FieldDeclaration] = Map()
    var algParams:Map[Operation,String] = Map()

    operations.sortWith(_.name < _.name).foreach(op => {
      val finalAlgebra:String = classify(m) + "ExpAlg"

      val str = s"""${op.name.capitalize}$finalAlgebra algebra${op.name.capitalize} = new ${op.name.capitalize}$finalAlgebra();"""
      algebraDeclarations = algebraDeclarations updated(op, Java(str).fieldDeclarations().head)
      algParams = algParams updated(op, s"algebra${op.name.capitalize}")
    })

    val str:String = s"""|$packageDeclaration
                         |import junit.framework.TestCase;
                         |
                         |public class TestSuite extends TestCase {
                         |    ${algebraDeclarations.values.mkString("\n")}
                         |  CombinedExpAlg algebra = new CombinedExpAlg(${algParams.values.mkString(",")});
                         |
                         |    ${unitTests.mkString("\n")}
                         |}""".stripMargin
    Java(str).compilationUnit()
  }

  /** Produce inner methods. */
  def innerMethod(tpe:expressions.Exp, operations:Seq[Operation]) : Seq[MethodDeclaration] = {
    var params:Seq[String] = Seq.empty
    var args:Seq[String] = Seq.empty

    tpe.attributes.foreach(att => {
      args = args :+ att.name
      if (att.tpe == types.Exp) {
        params = params :+ s"Combined ${att.name}" }
      else {
        params = params :+ typeGenerator(att.tpe) + s" ${att.name}"
      }
    })

    val opsname:Seq[MethodDeclaration] = operations.flatMap(op => {
      val returnType = typeGenerator(op.returnType.get)
      Java(
        s"public $returnType ${op.name}() { return algebra${op.name.capitalize}.${tpe.name.toLowerCase}(${args.mkString(",")}).${op.name}(); } ").methodDeclarations()
    })

    val str = s"""
                 |public Combined ${tpe.name.toLowerCase()}(${params.mkString(",")}) {
                 |		return new Combined() {
                 |			${opsname.mkString("\n")}
                 |		};
                 |	}
           """.stripMargin
    Java(str).methodDeclarations()
  }

  /** Combine all test cases together into a single JUnit 3.0 TestSuite class. */
  def combinedAlgebra(pack:Option[String], m:Model): CompilationUnit = {
    val operations:Seq[Operation] = m.flat().ops

    var algebraDeclarations:Map[Operation,FieldDeclaration] = Map()
    var paramDeclarations:Map[Operation,Statement] = Map()
    var argDeclarations:Map[Operation,String] = Map()
    var finalAlgebra:String = ""

    operations.foreach(op => {
      if (m.types.nonEmpty) {
        val combined = m.types.sortWith(_.name < _.name).map(op => op.name.capitalize).mkString("")
          .concat("ExpAlg")
        finalAlgebra = combined
      }

      finalAlgebra = classify(m) + "ExpAlg"

      algebraDeclarations = algebraDeclarations updated (op, Java(s"""${op.name.capitalize}$finalAlgebra algebra${op.name.capitalize};""").fieldDeclarations().head)
      paramDeclarations = paramDeclarations updated (op, Java(s"this.algebra${op.name.capitalize} = algebra${op.name.capitalize};").statement())
      argDeclarations = argDeclarations updated (op, s"${op.name.capitalize}$finalAlgebra algebra${op.name.capitalize}")
    })

    // must order the arguments for consistent usage.
    val argDeclarationsOrdered:String = argDeclarations.values.toSeq.sortWith(_ < _).mkString(",")
    val methods:Seq[MethodDeclaration] = m.flat().types.flatMap(exp => innerMethod(exp, operations))

    // operations has all operations
    val str:String =
      s"""
         |package algebra;
         |
         |// generated from all algebras
         |public class CombinedExpAlg implements $finalAlgebra<CombinedExpAlg.Combined> {
         |
         |	// combine together
         |	interface Combined extends ${operations.map(op => op.name.capitalize).mkString(",")} { }
         |
         |	// individual algebras, followed by combined one
         |
         |	${algebraDeclarations.values.mkString("\n")}
         |
         |	CombinedExpAlg ($argDeclarationsOrdered) {
         |		${paramDeclarations.values.mkString("\n")}
         |	}
         |
         |    ${methods.mkString("\n")}
         }""".stripMargin
    Java(str).compilationUnit()
  }
}








