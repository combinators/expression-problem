package org.combinators.models.original

import org.combinators.models.{ArgExpression, Expression}

/**
 * The first Model used to represent Dynamic Programming.
 * 
 * Avoid using this original prototype, and instead use @link{EnhancedModel}
 * 
 * @param problem  Uniquely identifies the problem
 * @param bounds   arguments to the class constructor, containing the input
 * @param cases    sequence of cases that linearly describes the problem
 * @param retrieveLabel  future access to retrieve solution generically
 */
class Model(val problem:String, 
            val bounds: List[ArgExpression], 
            val cases: List[(Option[Expression], Expression)], 
            val retrieveLabel: String = "take sub-solution")
