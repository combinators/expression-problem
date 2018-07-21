package example.expression.cpp

/*DI:LD:AI*/

import example.expression.domain.{BaseDomain, ModelDomain}

/**
  * Any C++ EP approach can extend this Generator
  */
trait AbstractGenerator  {
  val domain:BaseDomain with ModelDomain

  /** Retrieve model under consideration. */
  def getModel:domain.Model

  /** For the processed model, return generated code artifacts for solution. */
  def generatedCode():Seq[CPPFile]

  /**
    * Determines the C++ expression for all children of a Exp subtype based on its attributes.
    *
    * For example, an expressions.BinaryExp has 'left' and 'right' attributes, whereas an
    * expressions.UnaryExp only has an 'exp'
    */
  def subExpressions(exp:domain.Atomic) : Map[String, CPPElement]

  /** Responsible for dispatching sub-expressions with possible parameter(s). */
  def dispatch(expr:CPPElement, op:domain.Operation, params:CPPElement*) : CPPElement

  /**
    * Expression-tree data has attributes with domain-specific types. This method returns
    * the designated C++ type associated with the abstract type, with option of a covariant replacement
    */
  def typeConverter(tpe:domain.TypeRep, covariantReplacement:Option[CPPType] = None) : CPPType = {
    throw new scala.NotImplementedError(s"""Unknown Type "$tpe" """)
  }

  /**
    * Universal situation across all possible solutions is the sequence of statements that result
    * for a given Operation and data-type.
    */
  def logic(exp:domain.Atomic)(op:domain.Operation) : Seq[CPPElement] = {
    throw new scala.NotImplementedError(s"""Operation "${op.name}" does not handle case for sub-type "${exp.name}" """)
  }

  // Useful helper methods for any generator needing to craft common Java constructs

}
