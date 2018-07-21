package example.expression.haskell

/*DI:LD:AI*/

import example.expression.domain.{BaseDomain, ModelDomain}

/**
  * Any Haskell EP approach can extend this Generator
  */
trait AbstractGenerator  {
  val domain:BaseDomain with ModelDomain

  /** Retrieve model under consideration. */
  def getModel:domain.Model

  /** For the processed model, return generated code artifacts for solution. */
  def generatedCode():Seq[HaskellWithPath]

  /** If any new imports are needed for an operation, just extend here. */
  def addedImports(op:domain.Operation):Seq[Haskell] = Seq.empty

  /**
    * Determines the Haskell expression for all children of a Exp subtype based on its attributes.
    *
    * For example, an expressions.BinaryExp has 'left' and 'right' attributes, whereas an
    * expressions.UnaryExp only has an 'exp'
    */
  def subExpressions(exp:domain.Atomic) : Map[String, Haskell]

  /** Responsible for dispatching sub-expressions with possible parameter(s). */
  def dispatch(expr:Haskell, op:domain.Operation, params:Haskell*) : Haskell

  /**
    * Expression-tree data has attributes with domain-specific types. This method returns
    * the designated Haskell type associated with the abstract type, with option of a covariant replacement
    */
  def typeConverter(tpe:domain.TypeRep, covariantReplacement:Option[HaskellType] = None) : HaskellType = {
    throw new scala.NotImplementedError(s"""Unknown Type "$tpe" """)
  }

  /**
    * Universal situation across all possible solutions is the sequence of statements that result
    * for a given Operation and data-type.
    */
  def logic(exp:domain.Atomic)(op:domain.Operation) : Seq[Haskell] = {
    throw new scala.NotImplementedError(s"""Operation "${op.name}" does not handle case for sub-type "${exp.name}" """)
  }

  // Useful helper methods for any generator needing to craft common Java constructs

}
