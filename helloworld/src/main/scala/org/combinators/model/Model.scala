package org.combinators.model

import org.combinators.ep.domain.abstractions.TypeRep

trait ArgumentType { }

// problem instance types go here
case class BooleanType() extends ArgumentType
case class CharType() extends ArgumentType
case class IntegerType() extends ArgumentType
case class DoubleType() extends ArgumentType
case class StringType() extends ArgumentType

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
 *       for (int k = i; k < j; k++)



    if (i == j) { return 0; }

    int min = Integer.MAX_VALUE;  // could store in array and find min but then req extra storage
    for (int k = i; k < j; k++) {
      // compute cost of each possible starting point for a '(' between i and j-1 with a ')' at j
      int count = helper(i, k) + helper(k+1, j) + A[i-1] * A[k] * A[j];
      if (count < min) { min = count; decision.put(key(i,j), k); }
    }

    // Return minimum count
    return min;


 */
class Range(val variable:String, start:Expression, guard:Expression, advance:Expression)
class MinSubProblemDefinition(val params:Seq[Range], definition:Expression) extends Expression

abstract class Definition

abstract class DefinitionStatement
case class ExpressionStatement(expr:Expression) extends DefinitionStatement

case class IfThenElseDefinition(condition: Expression, result: DefinitionStatement, elseExpression: Definition) extends Definition
case class IfThenNoElseDefinition(condition: Expression, result: Expression, elseIfs: Seq[(Expression, Expression)]) extends Definition

case class MinRangeDefinition(
         args:Seq[HelperExpression],
         variable: HelperExpression,
         inclusiveStart: Expression,
         guardContinue:Expression,
         subproblemExpression: Expression,
         advance: Expression
) extends Definition

// just lift Expression
case class ExpressionDefinition(expr:Expression) extends Definition

trait ProblemOrder
case class Canonical() extends ProblemOrder
case class UpperTriangle() extends ProblemOrder

// trying a new approach that captures definitions. Each definition is in ordered sequence and specifies
// the essence of the problem
class EnhancedModel(val problem:String,
                    val input:Seq[ArgExpression],
                    val subproblemType:ArgumentType,        // Type of solution
                    val solutionType:ArgumentType,          // Type of return value
                    val solution:SubproblemInvocation,
                    val definition:Definition,
                    val mode:ProblemOrder = Canonical())

// Most DP problems solve subproblems in a canonical order, which is typified by a two-d array: solve rows first from top to bottom,
// and then within each row, columns from left to right
//
// Naturally not all follow this. one common alternative is solving problems along the upper triangle matrix. That is, first solve as
// the base case the long diagonal. Then solve all problems on the DP[i][j+1] for those entries just above the diagonal; then dp[i][j+2]
// for all values two above the diagonal and so on.

// Model has bounds and cases
// EnhancedModel has solution and definitions
