package org.combinators.model.enhancedModels

import org.combinators.model._

class PerfectSquare {
  def model: EnhancedModel = {
    // Needed for conditions and fib(n-1) and fib(n-2)
    val zero: LiteralInt = new LiteralInt(0)
    val one: LiteralInt = new LiteralInt(1)

    /* Perfect Square. */
    val n = new ArgExpression(0, "n", IntegerType(), "i")    // not sure if 'i' is used
    val bound_ps = List(n)

    val i: HelperExpression = HelperExpression("i", zero, SelfExpression("i") <= n, n + one)    // only one argument, i: not too sure this is right
    val k: HelperExpression = HelperExpression("k", zero, SelfExpression("k") * SelfExpression("k") <= n, n)    // not too sure this is right

    val helperTable_ps = Map("i" -> i, "k" -> k)            // Boy this is awkward: need k as helper but not in invocation
    val sol_ps = SubproblemInvocation(Seq("i"), helpers = helperTable_ps)

    val ps_subprobExpr = new SubproblemExpression(Seq(i - k * k)) + one
    val def_ps = MinRangeDefinition("k", one, k * k <= i, ps_subprobExpr, k + one)
    val ps_inner_definition =  IfThenElseDefinition(i == one, ExpressionStatement(one), def_ps)
    val ps_definition = IfThenElseDefinition(i == zero, ExpressionStatement(zero), ps_inner_definition)

    val PerfectSquare = new EnhancedModel("PerfectSquare",
      bound_ps,
      subproblemType = IntegerType(),    // helper method is an int
      solutionType   = StringType(), // solution is a string
      sol_ps,
      ps_definition,
      answer = ReturnExpressionDefinition(new SubproblemExpression(Seq(n)))
    )

    PerfectSquare
  }
}
