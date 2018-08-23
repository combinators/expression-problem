package example.expression.generator     /*DI:LI:AI*/

import example.expression.domain.{BaseDomain, ModelDomain}

trait LanguageIndependentGenerator {
  val domain:BaseDomain with ModelDomain

  /** Base concept for the representation of program unit on disk. */
  type CompilationUnit

  /** Base concept for a single expression in language. */
  type Expression

  /**  Base concept for a meaningful type in the language. */
  type Type

  /**  Base concept for a meaningful line-of-code in the language. */
  type Statement

  /** Retrieve model under consideration. */
  def getModel:domain.Model

  /** For the processed model, return generated code artifacts for solution. */
  def generatedCode():Seq[CompilationUnit]

  /**
    * Determines the Java expression for all children of a Exp subtype based on its attributes.
    *
    * For example, an expressions.BinaryExp has 'left' and 'right' attributes, whereas an
    * expressions.UnaryExp only has an 'exp'
    */
  def subExpressions(exp:domain.Atomic) : Map[String, Expression]

  /**
    * Responsible for dispatching sub-expressions with possible parameter(s).
    *
    * In an object-oriented-style of programming, there must be an 'expression' on which to base the dispatch.
    * Functional-oriented languages could ignore first
    */
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

}
