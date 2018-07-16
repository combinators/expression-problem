package example.expression.j  /*DI:LD:AI*/

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.`type`.Type
import com.github.javaparser.ast.body.{ConstructorDeclaration, FieldDeclaration, MethodDeclaration}
import com.github.javaparser.ast.expr.Expression
import com.github.javaparser.ast.stmt.Statement
import example.expression.domain.{BaseDomain, ModelDomain}
import org.combinators.templating.twirl.Java

/**
  * Any Java-based EP approach can extend this Generator
  */
trait AbstractGenerator  {
  val domain:BaseDomain with ModelDomain

  /** Retrieve model under consideration. */
  def getModel:domain.Model

  /**
    * Process model, as required by EP approach.
    *
    * Process the model as necessary. One could either (a) remove data types or operations that are nonsensical
    * for the given approach; or (b) flatten the hierarchy; (c) or use the default identify function.
    */
  //def getProcessedModel:domain.Model = getModel

  /** For the processed model, return generated code artifacts for solution. */
  def generatedCode():Seq[CompilationUnit]

  /**
    * Determines the Java expression for all children of a Exp subtype based on its attributes.
    *
    * For example, an expressions.BinaryExp has 'left' and 'right' attributes, whereas an
    * expressions.UnaryExp only has an 'exp'
    */
  def subExpressions(exp:domain.Atomic) : Map[String, Expression]

  /** Responsible for dispatching sub-expressions with possible parameter(s). */
  def dispatch(expr:Expression, op:domain.Operation, params:Expression*) : Expression

  /**
    * Expression-tree data has attributes with domain-specific types. This method returns
    * the designated Java type associated with the abstract type, with option of a covariant replacement
    */
  def typeConverter(tpe:domain.TypeRep, covariantReplacement:Option[Type] = None) : Type = {
    throw new scala.NotImplementedError(s"""Unknown Type "$tpe" """)
  }

  /**
    * Universal situation across all possible solutions is the sequence of statements that result
    * for a given Operation and data-type.
    */
  def logic(exp:domain.Atomic)(op:domain.Operation) : Seq[Statement] = {
    throw new scala.NotImplementedError(s"""Operation "${op.name}" does not handle case for sub-type "${exp.name}" """)
  }

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
