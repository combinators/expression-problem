package example.expression.j  /*DI:LD:AI*/

import com.github.javaparser.ast.body.{ConstructorDeclaration, FieldDeclaration, MethodDeclaration}
import example.expression.domain.{BaseDomain, ModelDomain}
import example.expression.generator.LanguageIndependentGenerator
import org.combinators.templating.twirl.Java

/**
  * Any Java-based EP approach can extend this Generator
  */
trait AbstractGenerator extends LanguageIndependentGenerator with DependentDispatch {
  val domain:BaseDomain with ModelDomain

  type CompilationUnit = com.github.javaparser.ast.CompilationUnit
  type Type = com.github.javaparser.ast.`type`.Type
  type Expression = com.github.javaparser.ast.expr.Expression
  type Statement = com.github.javaparser.ast.stmt.Statement

  // Useful helper methods for any generator needing to craft common Java constructs

  /** Generate constructor for given atomic concept, using suggested name */
  def constructor(exp:domain.Atomic, suggestedName:Option[String] = None, covariantOverride:Option[Type] = None) : ConstructorDeclaration = {
    val name = if (suggestedName.isEmpty) { exp.name } else { suggestedName.get }

    val atts:Seq[FieldDeclaration] = exp.attributes.flatMap(att => Java(s"private ${typeConverter(att.tpe, covariantOverride)} ${att.name};").fieldDeclarations())

    val params:Seq[String] = exp.attributes.map(att => s"${typeConverter(att.tpe, covariantOverride)} ${att.name}")
    val cons:Seq[Statement] = exp.attributes.flatMap(att => Java(s"  this.${att.name} = ${att.name};").statements())

    Java(s"""|public $name (${params.mkString(",")}) {
             |   ${cons.mkString("\n")}
             |}""".stripMargin).constructors().head
  }

  /** Generate constructor for given operation, using suggested name */
  def constructorFromOp(op:domain.Operation, suggestedName:Option[String] = None) : ConstructorDeclaration = {
    val name = if (suggestedName.isEmpty) { op.name.capitalize } else { suggestedName.get }

    val params:Seq[String] = op.parameters.map(p => s"${typeConverter(p._2)} ${p._1}")
    val cons:Seq[Statement] = op.parameters.flatMap(p => Java(s"  this.${p._1} = ${p._1};").statements())

    Java(
      s"""|public $name (${params.mkString(",")}) {
          |   ${cons.mkString("\n")}
          |}""".stripMargin).constructors().head
  }

  /** Compute parameter "name" comma-separated list from operation. */
  def arguments(op:domain.Operation) : String = {
    op.parameters.map(tuple => {
      val name:String = tuple._1

       name
    }).mkString(",")
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
