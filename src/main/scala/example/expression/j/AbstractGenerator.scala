package example.expression.j

import com.github.javaparser.ast.body. MethodDeclaration
import com.github.javaparser.ast.expr.Expression
import com.github.javaparser.ast.stmt.Statement
import example.expression.domain.Domain

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait AbstractGenerator {
  val domain:Domain
  import domain._

  /**
    * Determines the Java expression for all children of a Exp subtype based on its attributes.
    *
    * For example, an expressions.BinaryExp has 'left' and 'right' attributes, whereas an
    * expressions.UnaryExp only has an 'exp'
    */
  def subExpressions(exp:expressions.Exp) : Map[String, Expression]

  /** Responsible for dispatching sub-expressions. */
  def recurseOn(expr:Expression, op:Operation) : Expression

  /**
    * Expression-tree data has attributes with domain-specific types. This method returns
    * the designated Java type associated with the abstract type.
    */
  def typeGenerator(tpe:types.Types) : com.github.javaparser.ast.`type`.Type

  /** Operations are implemented as methods in the Base and sub-type classes. */
  def methodGenerator(exp:expressions.Exp)(op:Operation) : MethodDeclaration

  /**
    * Universal situation across all possible solutions is the sequence of statements that result
    * for a given Operation and datatype.
    */
  def methodBodyGenerator(exp:expressions.Exp)(op:Operation) : Seq[Statement]

  /**
    * Determine a potentially reduced model-chain that is compatible with a given generator.
    *
    * With no constraints, this is the identify function.
    */
  def compatible(model:Model):Model = model

}







