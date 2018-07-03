package example.expression.j

import com.github.javaparser.ast.expr.Expression
import com.github.javaparser.ast.stmt.Statement
import example.expression.domain.{BaseDomain, ModelDomain}
import org.combinators.templating.twirl.Java

/**
  * Each evolution has opportunity to enhance the code generators.
  */
trait AbstractGenerator  {
  val domain:BaseDomain with ModelDomain

  /**
    * Determines the Java expression for all children of a Exp subtype based on its attributes.
    *
    * For example, an expressions.BinaryExp has 'left' and 'right' attributes, whereas an
    * expressions.UnaryExp only has an 'exp'
    */
  def subExpressions(exp:domain.expressions.Exp) : Map[String, Expression]

  /** Provide ability to retrieve class associated with expression. */
  def getJavaClass() : Expression

  /** Responsible for dispatching sub-expressions. */
  //def recurseOn(expr:Expression, op:domain.Operation) : Expression

  /** Responsible for dispatching sub-expressions with possible parameter(s). */
  def recurseOn(expr:Expression, op:domain.Operation, params:Expression*) : Expression

  /**
    * For producer operations, there is a need to instantiate objects, and one would use this
    * method (with specific parameters) to carry this out.
    *
    * Almost got simplify to work with Interpreter solution. Only hold-up is that the instantiated
    * objects (i.e, "new Lit(0.0)") become more complex (i.e., a static factory method "lit(0.0)")
    *
    * I've crafted by hand, but don't want to break code tonight :)
    */
  def inst(exp:domain.expressions.Exp)(op:domain.Operation)(params:Expression*): Expression = {
    Java("new " + exp.name.capitalize + "(" + params.map(expr => expr.toString()).mkString(",") + ")").expression()
  }

  /**
    * Expression-tree data has attributes with domain-specific types. This method returns
    * the designated Java type associated with the abstract type.
    */
  def typeGenerator(tpe:domain.types.Types) : com.github.javaparser.ast.`type`.Type = {
    throw new scala.NotImplementedError(s"""Unknown Type "$tpe" """)
  }

  /**
    * Universal situation across all possible solutions is the sequence of statements that result
    * for a given Operation and data-type.
    */
  def methodBodyGenerator(exp:domain.expressions.Exp)(op:domain.Operation) : Seq[Statement] = {
    throw new scala.NotImplementedError(s"""Operation "${op.name}" does not handle case for sub-type "${exp.name}" """)
  }

  /**
    * Determine a potentially reduced model-chain that is compatible with a given generator.
    *
    * With no constraints, this is the identify function.
    */
  def compatible(model:domain.Model):domain.Model = model
}







