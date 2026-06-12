package org.combinators.models.enhancedModels.oneSequence

import org.combinators.models._

class MinCostClimbingStair {
  def model:EnhancedModel = {
    val zero: LiteralInt = LiteralInt(0)
    val one: LiteralInt = LiteralInt(1)
    val two: LiteralInt = LiteralInt(2)

    val costs  = ArgExpression(0, "costs", IntegerArrayType(), "i")
    val bounds = List(costs)

    // COULD be inferred from the ArgExpression list, but this lets us name variable to use in iterator
    val i: HelperExpression = HelperExpression("i", zero, SelfExpression("i") < ArrayLengthExpression(costs), ArrayLengthExpression(costs))

    // what the compute() method calls with helper(s1.length(), s2.length())
    val helpers = Map("i" -> i)
    val sol = SubproblemInvocation(order=Seq("i"), helpers = helpers, returnType = IntegerType())

    /*
     *   P(i,j,k) = 0, if i == 0 || j == 0 || k == 0 for all Ranges
     *   P(i,j,k) = Max of three subcases
     */

    // return cost[i] + Math.min(helper_topdown(i - 1),
    //                helper_topdown(i - 2));
    val recursive_case = ArrayElementExpression(costs, i) + MinExpression(SubproblemExpression(Seq(i - one)), SubproblemExpression(Seq(i - two)))

    // if (i == 0 || i == 1) {
    //            return cost[i];
    //        }
    val mccs_definition = IfThenElseDefinition((i == zero) || (i == one),
      ExpressionStatement(ArrayElementExpression(costs, i)),
      ExpressionDefinition(recursive_case))


    val MCCS = new EnhancedModel("MinCostClimbingStair",
      bounds,
      subproblemType = IntegerType(),         // helper() method returns int
      solutionType   = StringType(),          // solution is a string, showing where characters come from S1 with parens
      sol,
      mccs_definition,

      // how to determine answer
      answer = ReturnExpressionDefinition(TernaryExpression(ArrayLengthExpression(costs) == one,
        ArrayElementExpression(costs, zero),
        MinExpression(
          SubproblemExpression(Seq(ArrayLengthExpression(costs) - one)),
          SubproblemExpression(Seq(ArrayLengthExpression(costs) - two)))))
    )

    MCCS
  }
}
