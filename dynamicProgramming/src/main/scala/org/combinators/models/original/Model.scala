package org.combinators.models.original

import org.combinators.models.{ArgExpression, Expression}

/**
 * The first Model used to represent Dynamic Programming.
 * 
 * Avoid using this original prototype, and instead use @link{EnhancedModel}
 * 
 * @param problem
 * @param bounds
 * @param cases
 * @param retrieveLabel
 */
class Model(val problem:String, 
            val bounds: List[ArgExpression], 
            val cases: List[(Option[Expression], Expression)], 
            val retrieveLabel: String = "take sub-solution")
