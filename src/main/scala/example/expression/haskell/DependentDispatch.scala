package example.expression.haskell     /*DI:LD:AI*/

import example.expression.domain.{BaseDomain, ModelDomain}

/**
  * When an operation must dispatch a sub-expression to a dependent operator (i.e., when Simplify
  * calls Eval) this capability is used. Most generators will get this for free.
  *
  * ExtensibleVisitor pattern needs to dispatch carefully to dependent operators.
  */
trait DependentDispatch {
  val domain:BaseDomain with ModelDomain

  def dispatch(expr:Haskell, op:domain.Operation, params:Haskell*) : Haskell

 /**
   * Responsible for dispatching sub-expressions with possible parameter(s).
   */
  def dependentDispatch(expr:Haskell, op:domain.Operation, params:Haskell*) : Haskell = {
    dispatch(expr, op, params: _*)
  }
}
