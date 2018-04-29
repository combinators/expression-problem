package example.expression.covariant

import example.expression.ExpressionDomain
import expression._
import expression.instances.UnitSuite


/** Future work to sanitize combinators to be independent of Exp. */
class ExpressionSynthesis(override val domain:DomainModel, val tests:UnitSuite) extends ExpressionDomain(domain, tests) with SemanticTypes {


}
