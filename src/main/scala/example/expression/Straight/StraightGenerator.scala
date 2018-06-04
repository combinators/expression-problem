package example.expression.Straight

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.body.{FieldDeclaration, MethodDeclaration}
import com.github.javaparser.ast.expr.Expression
import com.github.javaparser.ast.stmt.Statement
import example.expression.domain.Domain
import org.combinators.templating.twirl.Java

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait StraightGenerator {
  val domain:Domain
  import domain._

  /** Request given operation on the Java identifier. */
  def oper(expVar:String, op:Operation): Expression = {
    Java(s"$expVar.${op.name}()").expression()
  }

  /** Return designated Java type associated with type, or void if all else fails. */
  def typeGenerator(tpe:types.Types) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case types.Exp => Java("Exp").tpe()
      case _ => Java ("void").tpe()  // reasonable stop
    }
  }

  /** Operations are implemented as methods in the Base and sub-type classes. */
  def methodGenerator(exp:expressions.Exp)(op:Operation): MethodDeclaration = {
    val retType = op.returnType match {
      case Some(tpe) => typeGenerator(tpe)
      case _ => Java("void").tpe
    }

    Java(s"""|public $retType ${op.name}() {
             |  ${methodBodyGenerator(exp)(op).mkString("\n")}
             |}""".stripMargin).methodDeclarations().head
  }

  def methodBodyGenerator(exp:expressions.Exp)(op:Operation): Seq[Statement] = {
    throw new scala.NotImplementedError(s"""Operation "${op.name}" does not handle case for sub-type "${exp.getClass.getSimpleName}" """)
  }

  /** Generate the full class for the given expression sub-type. */
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
            |public class $name extends Exp {
            |
            |  ${constructor.toString}
            |
            |  ${atts.mkString("\n")}
            |
            |  ${methods.mkString("\n")}
            |}""".stripMargin).compilationUnit()
  }

  /** Generate the base class. */
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
    Java(s"""|public abstract class Exp {
             |  ${signatures.mkString("\n")}
             |}""".stripMargin).compilationUnit()
  }
}







