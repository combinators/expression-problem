package example.expression.algebra

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.body.{FieldDeclaration, MethodDeclaration}
import com.github.javaparser.ast.expr.Expression
import com.github.javaparser.ast.stmt.Statement
import example.expression.domain.{BaseDomain, MathDomain, ModelDomain}
import example.expression.j.{TestGenerator, TestGeneratorWithModel}
import org.combinators.templating.twirl.Java

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait AlgebraTestGenerator extends TestGeneratorWithModel {
  val domain: BaseDomain with ModelDomain

  // exists from AbstractGenerator
  def typeConverter(tpe:domain.TypeRep) : com.github.javaparser.ast.`type`.Type

  /** Convert a test instance into a Java Expression for instantiating that instance. */
  override def convert(inst: domain.AtomicInst): Expression = {
    val name = inst.e.name
    val opname = name.toLowerCase()
    inst match {
      //case lit: domain.LitInst => Java(s"algebra.lit(${lit.i.get.toString})").expression()
      case ui: domain.UnaryInst =>
        Java(s"algebra.$opname(${convert(ui.inner)})").expression()
      case bi: domain.BinaryInst =>
        Java(s"algebra.$opname(${convert(bi.left)}, ${convert(bi.right)})").expression()
      case exp:domain.AtomicInst => Java(s"algebra.lit(${exp.i.get.toString})").expression()

      case _ => Java(s""" "unknown $name" """).expression()
    }
  }

  /**
    * Classify model based on current or most recently defined types.
    *
    * The classification is a sorted concatenation of the most recent model (including self) that
    * defines new data types.
    */
  def classify(m:domain.Model) : String = {
    if (m.isEmpty) {
      return ""
    }
    if (m.types.isEmpty) {
      return classify(m.last)
    }

    m.types.sortWith(_.name < _.name).map(op => op.name.capitalize).mkString("")
  }

  /** Combine all test cases together into a single JUnit 3.0 TestSuite class. */
  override def generateSuite(pack: Option[String]): CompilationUnit = {
    val methods: Seq[MethodDeclaration] = testGenerator

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
    val operations: Seq[domain.Operation] = getModel.flat().ops
    var algebraDeclarations: Map[domain.Operation, FieldDeclaration] = Map()
    var algParams:Map[domain.Operation,String] = Map()

    operations.sortWith(_.name < _.name).foreach(op => {
      val finalAlgebra:String = classify(getModel) + "ExpAlg"

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
  def innerMethod(tpe:domain.Atomic, operations:Seq[domain.Operation]) : Seq[MethodDeclaration] = {
    var params:Seq[String] = Seq.empty
    var args:Seq[String] = Seq.empty

    tpe.attributes.foreach(att => {
      args = args :+ att.name
      if (att.tpe == domain.baseTypeRep) {
        params = params :+ s"Combined ${att.name}" }
      else {
        params = params :+ typeConverter(att.tpe) + s" ${att.name}"
      }
    })

    val opsname:Seq[MethodDeclaration] = operations.flatMap(op => {
      val returnType = typeConverter(op.returnType.get)
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
  def combinedAlgebra(pack:Option[String], m:domain.Model): CompilationUnit = {
    val operations:Seq[domain.Operation] = m.flat().ops

    var algebraDeclarations:Map[domain.Operation,FieldDeclaration] = Map()
    var paramDeclarations:Map[domain.Operation,Statement] = Map()
    var argDeclarations:Map[domain.Operation,String] = Map()
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








