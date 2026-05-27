package org.combinators.models

trait ArgumentType {
  
}

// problem instance types go here
case class BooleanType() extends ArgumentType
case class CharType() extends ArgumentType
case class IntegerType() extends ArgumentType
case class DoubleType() extends ArgumentType
case class StringType() extends ArgumentType
case class UnitType() extends ArgumentType 

// possibly choose to make this Generic but that seems like overkill
trait ArrayType(val elementType:ArgumentType) extends ArgumentType

case class IntegerArrayType() extends ArrayType(IntegerType())
case class IntegerArray2DType() extends ArrayType(IntegerArrayType())

// an array of string values
case class StringArrayType() extends ArrayType(StringType())

class Argument (val argName:String, val argType:ArgumentType)

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
class MinSubProblemDefinition(val params:Seq[Range], definition:Expression) extends Expression {
  def tpe:ArgumentType = definition.tpe
}

abstract class Definition

abstract class DefinitionStatement
case class ExpressionStatement(expr:Expression) extends DefinitionStatement

case class IfThenElseDefinition(condition: Expression, result: DefinitionStatement, elseExpression: Definition) extends Definition
case class IfThenNoElseDefinition(condition: Expression, result: Expression, elseIfs: Seq[(Expression, Expression)]) extends Definition

// Set P(...) = compute sum of a range of other P(...) based on one-dimensional range starting at inclusiveStart
case class SumDefinition(
  variable: String,
  inclusiveStart: Expression,
  guardContinue:Expression,
  subproblemExpression: Expression,
  advance: Expression
) extends Definition

case class AccumulatorInformation(variable:String, inclusiveStart:Expression, guardCondition:Expression, advance:Expression)

//ReturnAccumulatedDefinition("idx", "sum", zero, SelfExpression("idx") <= n, SubproblemExpression(Seq(n, SelfExpression("idx"))), SelfExpression("idx") + one)
// Accumulate values of P(...) and return as Integer
case class ReturnAccumulatedDefinition(
  accumulationVariable: String,
  iteration: Seq[AccumulatorInformation],  
  subproblemExpression: Expression,
) extends Definition

case class MinRangeDefinition(
         variable: String,
         inclusiveStart: Expression,
         guardContinue:Expression,
         subproblemExpression: Expression,
         advance: Expression
) extends Definition

case class MaxRangeDefinition(
         variable: String,
         inclusiveStart: Expression,
         guardContinue:Expression,
         subproblemExpression: Expression,
         advance: Expression
       ) extends Definition

// just lift Expression
case class ExpressionDefinition(expr:Expression) extends Definition

// for Top-down, just return; for Bottom-Up, return assigned var
case class ReturnExpressionDefinition(expr:Expression) extends Definition

trait ProblemOrder
case class Canonical() extends ProblemOrder
case class UpperTriangle(params:Seq[String]) extends ProblemOrder

// trying a new approach that captures definitions. Each definition is in ordered sequence and specifies
// the essence of the problem
class EnhancedModel(val problem:String,
                    val input:Seq[ArgExpression],
                    val subproblemType:ArgumentType,        // Type of solution
                    val solutionType:ArgumentType,          // Type of return value
                    val solution:SubproblemInvocation,
                    val definition:Definition,
                    val answer:Definition,                  // all existing Expression should just use ExpressionDefinition(expr)
                    val mode:ProblemOrder = Canonical()) {

  def find(variable:String) : HelperExpression = {
    // first locate in solution.params
    if (solution.helpers.contains(variable)) {
      solution.helpers(variable)
    } else {
      ???
    }
  }

  // allows mapping to affect how DP problems are stored, typically in bottom up
  def find_map(variable:String) : Expression = {
    if (solution.mappers.contains(variable)) {
      solution.mappers(variable) // overrides the default
    } else {
      find(variable)
    }
  }
}

// Most DP problems solve subproblems in a canonical order, which is typified by a two-d array: solve rows first from top to bottom,
// and then within each row, columns from left to right
//
// Naturally, not all follow this. one common alternative is solving problems along the upper triangle matrix. That is, first solve as
// the base case the long diagonal. Then solve all problems on the DP[i][j+1] for those entries just above the diagonal; then dp[i][j+2]
// for all values two above the diagonal and so on.

// Model has bounds and cases
// EnhancedModel has solution and definitions
