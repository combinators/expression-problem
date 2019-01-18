package example.expression.j     /*DI:LD:AI*/

import com.github.javaparser.ast.expr.Expression
import example.expression.domain.{BaseDomain, ModelDomain}

/**
  * At times one wants to perform a different operation on a given context.
  *
  * Two cases come to mind:
  *
  * 1. When an operation must dispatch a sub-expression to a dependent operator (i.e.,
  * when Simplify calls Eval on a left child) this capability is used. Most generators
  * will get this for free, since it can usually be handled by dispatch, but not always.
  *
  * 2. When an operation must dispatch current context to a dependent operator (i.e.,
  * when Equals requests to call Astree on two structures).
  *
  * ExtensibleVisitor pattern needs to dispatch carefully to dependent operators.
  */
trait DependentDispatch {
  val domain:BaseDomain with ModelDomain

  def dispatch(expr:Expression, op:domain.Operation, params:Expression*) : Expression

 /**
   * Responsible for dispatching sub-expressions with possible parameter(s).
   */
  def dependentDispatch(expr:Expression, op:domain.Operation, params:Expression*) : Expression = {
    dispatch(expr, op, params: _*)
  }
}
