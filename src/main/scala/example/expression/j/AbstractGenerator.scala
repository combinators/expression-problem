package example.expression.j

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.`type`.Type
import com.github.javaparser.ast.expr.Expression
import com.github.javaparser.ast.stmt.Statement
import example.expression.domain.{BaseDomain, ModelDomain}

/**
  * Any Java-based EP approach can extend this Generator
  */
trait AbstractGenerator  {
  val domain:BaseDomain with ModelDomain

  /**
    * Process model, as required by EP approach.
    *
    * Process the model as necessary. One could either (a) remove data types or operations that are nonsensical
    * for the given approach; or (b) flatten the hierarchy; (c) or use the default identify function.
    */
  def apply(model:domain.Model):domain.Model = model

  /** For the processed model, return generated code artifacts for solution. */
  def generatedCode(model:domain.Model):Seq[CompilationUnit]

  /**
    * Determines the Java expression for all children of a Exp subtype based on its attributes.
    *
    * For example, an expressions.BinaryExp has 'left' and 'right' attributes, whereas an
    * expressions.UnaryExp only has an 'exp'
    */
  def subExpressions(exp:domain.Atomic) : Map[String, Expression]

  /** Responsible for dispatching sub-expressions with possible parameter(s). */
  def recurseOn(expr:Expression, op:domain.Operation, params:Expression*) : Expression

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


}
