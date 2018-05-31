package example.expression.Pure

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.body.{FieldDeclaration, MethodDeclaration}
import com.github.javaparser.ast.expr.Expression
import com.github.javaparser.ast.stmt.Statement
import org.combinators.templating.twirl.Java

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait AbstractGenerator {
  val pure:Pure
  import pure._

  /** Return designated Java type associated with type, or void if all else fails. */
  def typeGenerator(tpe:types.Types) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case types.Exp => Java(s"$BASE").tpe()
      case _ => Java ("void").tpe()  // reasonable stop
    }
  }

  def methodGenerator(exp:expressions.Exp)(op:Operation): MethodDeclaration = {
    val retType = op.returnType match {
      case Some(tpe) => typeGenerator(tpe)
      case _ => Java("void").tpe
    }

    Java(s"""|public $retType ${op.name}() {
             |  ${methodBodyGenerator(exp)(op).mkString("\n")}
             |}""".stripMargin).methodDeclarations().head
  }

  def methodBodyGenerator(exp:expressions.Exp)(op:Operation): Seq[Statement] = ???

  /** Return sample JUnit test cases. */
  def testGenerator(): Seq[MethodDeclaration] = Seq.empty

  /** Convert a test instance into a Java Expression for instantiating that instance. */
  def convert(inst:instances.ExpInst) : Expression = {
    var name:String = inst.e.getClass.getSimpleName
    name = name.substring(0, name.length-1)

    inst match {
      case lit:LitInst => Java(s"new $name(${lit.i.get.toString})").expression()
      case ui:instances.UnaryExpInst =>
        Java(s"new $name(${convert(ui.exp)})").expression()
      case bi:instances.BinaryExpInst =>
        Java(s"new $name(${convert(bi.left)}, ${convert(bi.right)})").expression()
      case _ =>  Java(s""" "unknown $name" """).expression()
    }
  }

  def generateSuite(): CompilationUnit = {
    val methods:Seq[MethodDeclaration] = testGenerator()

    var num:Int = 0
    val unitTests:Seq[MethodDeclaration] = methods.filter(md => md.getBody.isPresent).map(md => {
      num = num + 1
      Java (s"""public void test$num()  ${md.getBody.get.toString} """).methodDeclarations().head
    })

    Java(s"""|import junit.framework.TestCase;
             |public class TestSuite extends TestCase {
             |    ${unitTests.mkString("\n")}
             |}""".stripMargin).compilationUnit()
  }

  def generateExp(domain:Model, e:expressions.Exp) : CompilationUnit = {
    val name = e.toString

    val methods:Seq[MethodDeclaration] = domain.ops.map(methodGenerator(e))
    val atts:Seq[FieldDeclaration] = e.attributes.flatMap(att => Java(s"private ${typeGenerator(att.tpe)} ${att.name};").fieldDeclarations())

    // Builds up the attribute fields and set/get methods. Also prepares for one-line constructor.
    var params:Seq[String] = Seq.empty
    var cons:Seq[String] = Seq.empty

    val fields:Seq[FieldDeclaration] = e.attributes.map(att => {
      val capAtt = att.name.capitalize
      val tpe = typeGenerator(att.tpe)

      params = params :+ s"$tpe ${att.name}"
      cons   = cons   :+ s"  this.${att.name} = ${att.name};"

      Java(s"private $tpe ${att.name};").fieldDeclarations().head
    })

    val constructor = Java(s"""|public $name (${params.mkString(",")}) {
                               |   ${cons.mkString("\n")}
                               |}""".stripMargin).constructors().head

    Java(s"""
            |public class $name extends $BASE {
            |
            |  ${constructor.toString}
            |
            |  ${atts.mkString("\n")}
            |
            |  ${methods.mkString("\n")}
            |}""".stripMargin).compilationUnit()
  }

  def generateBase(domain:Model): CompilationUnit = {

    // Allow for operations to be void; if not an issue in future, then filter out here...
    val signatures:Seq[MethodDeclaration] = domain.ops.map(op => {

      val ret = if (op.returnType.isDefined) {
        typeGenerator(op.returnType.get)
      } else {
        "void"
      }

      Java(s"public abstract $ret ${op.name}();").methodDeclarations().head
    })

    // same every time
    Java(s"""|public abstract class $BASE {
             |  ${signatures.mkString("\n")}
             |}""".stripMargin).compilationUnit()
  }
}







