package example.expression.j

import com.github.javaparser.ast.body.{ConstructorDeclaration, FieldDeclaration, MethodDeclaration}
import com.github.javaparser.ast.stmt.Statement
import com.github.javaparser.ast.`type`.Type
import org.combinators.templating.twirl.Java

/**
  * Useful constructs for creating Java attributes, methods, and constructors.
  */
trait JavaGenerator extends AbstractGenerator {

  /** Generate constructor for given atomic concept, using suggested name */
  def constructor(exp:domain.Atomic, suggestedName:String = "") : ConstructorDeclaration = {
    val name = if (suggestedName.equals("")) { exp.name } else { suggestedName }

    val atts:Seq[FieldDeclaration] = exp.attributes.flatMap(att => Java(s"private ${typeConverter(att.tpe)} ${att.name};").fieldDeclarations())

    val params:Seq[String] = exp.attributes.map(att => s"${typeConverter(att.tpe)} ${att.name}")
    val cons:Seq[Statement] = exp.attributes.flatMap(att => Java(s"  this.${att.name} = ${att.name};").statements())

    Java(s"""|public $name (${params.mkString(",")}) {
             |   ${cons.mkString("\n")}
             |}""".stripMargin).constructors().head
  }

  /** Compute parameter "Type name" comma-separated list from operation. */
  def parameters(op:domain.Operation) : String = {
    op.parameters.map(tuple => {
      val name:String = tuple._1
      val tpe:domain.TypeRep = tuple._2

      typeConverter(tpe).toString + " " + name
    }).mkString(",")
  }

  /**
    * Produce all getter methods for the given exp, with suitable possibiity of using covariant replacement
    * on domain.BaseTypeRep
    */
  def getters(exp:domain.Atomic, covariantOverride:Option[Type] = None) : Seq[MethodDeclaration] =

    exp.attributes.flatMap(att => Java(s"""|public ${typeConverter(att.tpe, covariantOverride)} get${att.name.capitalize}() {
                                           |    return this.${att.name};
                                           |}""".stripMargin).methodDeclarations())

  /**
    * Given an exp, produce a sequence of field declarations, for each of the attributes.
    *
    * @param exp
    * @param covariantOverride    Optional cass to use as covariant overriding class for BaseTypeExp
    * @return
    */
  def fields(exp:domain.Atomic, covariantOverride:Option[Type] = None) : Seq[FieldDeclaration] = {
    exp.attributes.flatMap(att => Java(s"private ${typeConverter(att.tpe, covariantOverride)} ${att.name};").fieldDeclarations())
  }
}
