package example.expression.scala   /*DI:LD:AI*/

import example.expression.domain.{BaseDomain, ModelDomain}
import scala.meta._

/**
  * When an operation must dispatch a sub-expression to a dependent operator (i.e., when Simplify
  * calls Eval) this capability is used. Most generators will get this for free.
  *
  * ExtensibleVisitor pattern needs to dispatch carefully to dependent operators.
  */
trait DependentDispatch {
  val domain:BaseDomain with ModelDomain

  def dispatch(expr:Term, op:domain.Operation, params:Term*) : Term

 /**
   * Responsible for dispatching sub-expressions with possible parameter(s).
   */
  def dependentDispatch(expr:Term, op:domain.Operation, params:Term*) : Term = {
    dispatch(expr, op, params: _*)
  }
}
