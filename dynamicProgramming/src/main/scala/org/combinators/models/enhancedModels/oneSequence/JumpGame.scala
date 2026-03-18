package org.combinators.models.enhancedModels.oneSequence

import org.combinators.models._

class JumpGame {

  def model: EnhancedModel = {

    val zero = new LiteralInt(0)
    val one  = new LiteralInt(1)

    val arr = new ArgExpression(0, "arr", IntegerArrayType(), "j")  // j is the iterator
    //val index = new ArgExpression(1, "i", IntegerType(), "i")
    val arr_length = new ArrayLengthExpression(arr)
    //val eleAtIndex = new ArrayElementExpression(arr, index)
    //val loopBound = new AdditionExpression(eleAtIndex, index)
    val bound = List(arr)

    val i:HelperExpression = HelperExpression("i", one, SelfExpression("i") <= arr_length, arr_length + one)

    val result_min =
      MinRangeDefinition(
        "j",
        i +  one,
        SelfExpression("j") <= arr_length,
        new SubproblemExpression(Seq(SelfExpression("j"))),
        new AdditionExpression(SelfExpression("j"), one)
      )

    val helperTable = Map("i" -> i)

    val sol_dt = SubproblemInvocation(Seq("i"), helpers = helperTable)

    val basecase = IfThenElseDefinition(arr_length - one <= arr_length, ExpressionStatement(zero), result_min)

    val JumpGame = new EnhancedModel("JumpGame",
      bound,
      subproblemType = IntegerType(),
      solutionType = IntegerType(),
      solution = sol_dt,
      definition = basecase,
      answer = ReturnExpressionDefinition(new SubproblemExpression((Seq(zero))))
    )

    JumpGame
  }
}