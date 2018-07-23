package example.expression.j     /*DI:LD:AI*/

import com.github.javaparser.ast.expr.Expression
import example.expression.domain.{BaseDomain, ModelDomain}

/**
  * When an operation must dispatch a sub-expression to a dependent operator (i.e., when Simplify
  * calls Eval) this capability is used. Most generators will get this for free.
  *
  * ExtensibleVisitor pattern needs to dispatch carefully to dependent operators.
  */
trait DependentDispatch {
  val domain:BaseDomain with ModelDomain

  def dispatch(expr:Expression, op:domain.Operation, params:Expression*) : Expression

 /**
   * Responsible for dispatching sub-expressions with possible parameter(s).
   *
   */
  def dependentDispatch(expr:Expression, op:domain.Operation, params:Expression*) : Expression = {
    dispatch(expr, op, params: _*)
  }
}
