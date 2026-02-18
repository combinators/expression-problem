package org.combinators.model.enhancedModels

import org.combinators.model._

class MinCostClimbingStair {
  def model:EnhancedModel = {
    val zero: LiteralInt = new LiteralInt(0)
    val one: LiteralInt = new LiteralInt(1)
    val two: LiteralInt = new LiteralInt(2)

    val costs  = new ArgExpression(0, "costs", IntegerArrayType(), "i")
    val bounds = List(costs)

    // COULD be inferred from the ArgExpression list, but this lets us name variable to use in iterator
    val i: HelperExpression = HelperExpression("i", zero, SelfExpression("i") < new ArrayLengthExpression(costs), new ArrayLengthExpression(costs))

    // what the compute() method calls with helper(s1.length(), s2.length())
    val helpers = Map("i" -> i)
    val sol = SubproblemInvocation(order=Seq("i"), helpers = helpers, returnType = IntegerType())

    /*
     *   P(i,j,k) = 0, if i == 0 || j == 0 || k == 0 for all Ranges
     *   P(i,j,k) = Max of three sub-cases
     */

    // return cost[i] + Math.min(helper_topdown(i - 1),
    //                helper_topdown(i - 2));
    val recursive_case = new ArrayElementExpression(costs, i) + new MinExpression(new SubproblemExpression(Seq(i - one)), new SubproblemExpression(Seq(i - two)))

    // if (i == 0 || i == 1) {
    //            return cost[i];
    //        }
    val mccs_definition = IfThenElseDefinition((i == zero) || (i == one),
      ExpressionStatement(new ArrayElementExpression(costs, i)),
      ExpressionDefinition(recursive_case))


    val MCCS = new EnhancedModel("MinCostClimbingStair",
      bounds,
      subproblemType = IntegerType(),         // helper() method returns int
      solutionType   = StringType(),          // solution is a string, showing where characters come from S1 with parens
      sol,
      mccs_definition,

      // how to determine answer
      answer = new TernaryExpression(new ArrayLengthExpression(costs) == one,
        new ArrayElementExpression(costs, zero),
        new MinExpression(
          new SubproblemExpression(Seq(new ArrayLengthExpression(costs) - one)),
          new SubproblemExpression(Seq(new ArrayLengthExpression(costs) - two))))
    )

    MCCS
  }
}
