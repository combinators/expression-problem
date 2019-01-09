package example.expression.algebra /*DI:LD:AD*/

import com.github.javaparser.ast.body.{FieldDeclaration, MethodDeclaration}
import example.expression.domain.{BaseDomain, ModelDomain}
import example.expression.generator.LanguageIndependentTestGenerator
import example.expression.j.{JUnitTestGenerator, JavaBinaryMethod, JavaGenerator}
import org.combinators.templating.twirl.Java

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait AlgebraTestGenerator extends JUnitTestGenerator with JavaGenerator with LanguageIndependentTestGenerator with JavaBinaryMethod {
  val domain: BaseDomain with ModelDomain
  import domain._

  /** Convert a test instance into a Java Expression for instantiating that instance. */
  override def convert(inst: AtomicInst): Expression = {
    val name = inst.e.name
    val opname = name.toLowerCase()
    inst match {
      //case lit: domain.LitInst => Java(s"algebra.lit(${lit.i.get.toString})").expression()
      case ui: UnaryInst =>
        Java(s"algebra.$opname(${convert(ui.inner)})").expression()
      case bi: BinaryInst =>
        Java(s"algebra.$opname(${convert(bi.left)}, ${convert(bi.right)})").expression()
      case exp:AtomicInst => Java(s"algebra.lit(${exp.i.get.toString})").expression()

      case _ => Java(s""" "unknown $name" """).expression()
    }
  }

  /** Type to use when referring to specific instance. */
  override def exprDefine(exp:AtomicInst) : Type = {
    Java(s"Combined${domain.baseTypeRep.name}Alg.Combined").tpe()
  }

  /** Used when one already has code fragments bound to variables, which are to be used for left and right. */
  override def convertRecursive(inst: Binary, left:String, right:String): Expression = {
    Java(s"algebra.${inst.name.toLowerCase} ($left, $right)").expression()
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
  override def generateSuite(pkg: Option[String], m:Option[Model] = None): Seq[CompilationUnit] = {
    val methods: Seq[MethodDeclaration] = testGenerator ++ performanceMethod()

    val packageDeclaration: String = if (pkg.isDefined) {
      s"package ${pkg.get};"
    } else {
      ""
    }

    var num: Int = 0
    val unitTests: Seq[CompilationUnit] = methods.filter(md => md.getBody.isPresent).map(md => {
      num = num + 1
      //Java(s"public void test$num() ${md.getBody.get.toString}").methodDeclarations().head

      // must get all operations defined for this model and earlier. For each one, define algebra with
      // current extension
      val model = m.getOrElse(emptyModel())
      val operations: Seq[Operation] = model.flatten().ops
      var algebraDeclarations: Map[Operation, FieldDeclaration] = Map()
      var algParams:Map[Operation,String] = Map()

      // likely sorting is not useful here...
      operations.sortWith(_.name < _.name).foreach(op => {
        val finalAlgebra:String = classify(model) + s"${domain.baseTypeRep.name}Alg"

        val str = s"""${op.name.capitalize}$finalAlgebra algebra${op.name.capitalize} = new ${op.name.capitalize}$finalAlgebra();"""
        algebraDeclarations = algebraDeclarations updated(op, Java(str).fieldDeclarations().head)
        algParams = algParams updated(op, s"algebra${op.name.capitalize}")
      })

      // sort by class name
      val sortedParams:String = algParams.values.toSeq.sortWith(_ < _).mkString(",")

      val str:String = s"""|$packageDeclaration
                           |import junit.framework.TestCase;
                           |
                           |public class TestSuite$num extends TestCase {
                           |  ${algebraDeclarations.values.mkString("\n")}
                           |  Combined${domain.baseTypeRep.name}Alg algebra = new Combined${domain.baseTypeRep.name}Alg($sortedParams);
                           |
                           |  $md
                           |}""".stripMargin
      Java(str).compilationUnit()
    })

    unitTests
  }

  /** Produce inner methods. */
  def innerMethod(tpe:Atomic, operations:Seq[Operation]) : Seq[MethodDeclaration] = {
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
      val op_args = arguments(op)
      //val op_params = parameters(op)

      // Handle binary methods...
      val op_params = op match {
        case bm:domain.BinaryMethod => binaryMethodParameters(op, typeConverter)
        case _ => parameters(op)
      }


      val returnType = typeConverter(op.returnType.get)
      Java(
        s"public $returnType ${op.name}($op_params) { return algebra${op.name.capitalize}.${tpe.name.toLowerCase}(${args.mkString(",")}).${op.name}($op_args); } ").methodDeclarations()
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
    val operations:Seq[Operation] = m.flatten().ops

    var algebraDeclarations:Map[Operation,FieldDeclaration] = Map()
    var paramDeclarations:Map[Operation,Statement] = Map()
    var argDeclarations:Map[Operation,String] = Map()
    var finalAlgebra:String = ""

    operations.foreach(op => {
      if (m.types.nonEmpty) {
        val combined = m.types.sortWith(_.name < _.name).map(op => op.name.capitalize).mkString("")
          .concat(s"${domain.baseTypeRep.name}Alg")
        finalAlgebra = combined
      }

      finalAlgebra = classify(m) + s"${domain.baseTypeRep.name}Alg"

      algebraDeclarations = algebraDeclarations updated (op, Java(s"""${op.name.capitalize}$finalAlgebra algebra${op.name.capitalize};""").fieldDeclarations.head)
      paramDeclarations = paramDeclarations updated (op, Java(s"this.algebra${op.name.capitalize} = algebra${op.name.capitalize};").statement)
      argDeclarations = argDeclarations updated (op, s"${op.name.capitalize}$finalAlgebra algebra${op.name.capitalize}")
    })

    // must order the arguments for consistent usage.
    val argDeclarationsOrdered:String = argDeclarations.values.toSeq.sortWith(_ < _).mkString(",")
    val methods:Seq[MethodDeclaration] = m.flatten().types.flatMap(exp => innerMethod(exp, operations))

    // operations has all operations
    val str:String = s"""
         |package algebra;
         |
         |// generated from all algebras
         |public class Combined${domain.baseTypeRep.name}Alg implements $finalAlgebra<Combined${domain.baseTypeRep.name}Alg.Combined> {
         |
         |	// combine together
         |	interface Combined extends ${operations.map(op => op.name.capitalize).mkString(",")} { }
         |
         |	// individual algebras, followed by combined one
         |	${algebraDeclarations.values.mkString("\n")}
         |
         |	Combined${domain.baseTypeRep.name}Alg ($argDeclarationsOrdered) {
         |		${paramDeclarations.values.mkString("\n")}
         |	}
         |
         |  ${methods.mkString("\n")}
         }""".stripMargin
    Java(str).compilationUnit()
  }
}
