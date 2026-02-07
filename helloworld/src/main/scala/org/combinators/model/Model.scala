package org.combinators.model

import org.combinators.ep.domain.abstractions.TypeRep

trait ArgumentType { }

// problem instance types go here
class IntegerType extends ArgumentType
class StringType extends ArgumentType
class CharType extends ArgumentType
class DoubleType extends ArgumentType

// possibly choose to make this Generic but that seems like overkill
class IntegerArrayType extends ArgumentType

class Argument (val argName:String, val argType:ArgumentType)

class Model(val problem:String, val bounds: List[ArgExpression], val cases: List[(Option[Expression], Expression)])

// test example in bottomUp/twoSequences/longestcommonsubsequence/LongestCommonSubsequenceObjectOrientedProvider.scala

/**
 * For MatrixChain Multiplication, whether top-down or bottom up, the key formulation is:
 *
 * Input:
 *    A:IntegerArrayType
 *
 * SolutionType:
 *    LiteralString
 *
 * Solution:
 *    P(1, A.length-1)         From this start, should be able to infer the double loop from MinSubProblemDefinition
 *
 * Decomposed Solutions:
 *
 *    P(i,j) = 0, if i == j
 *    P(i,j) = Min (k, P(i,k) + P(k+1,j) + cost of multiplying resulting two matrices)
 *       for (int i = 1; i < A.length-1; i++), (int k = i; k < j; k++)
 *
 */
class Range(val variable:String, start:Expression, guard:Expression, advance:Expression)
class MinSubProblemDefinition(val params:Seq[Range], definition:Expression) extends Expression
class Definition(val args:Seq[ArgExpression], /* val constraint:Constraint, */val result:Expression)   // might not need to be extended?

// trying a new approach that captures definitions. Each definition is in ordered sequence and specifies
// the essence of the problem
class EnhancedModel(val problem:String,
                    val input:Seq[ArgExpression],
                    solutionType:LiteralExpression,
                    solution:SubproblemExpression,
                    definitions:Seq[Definition])
