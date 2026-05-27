package org.combinators.models.enhancedModels.oneSequence

import org.combinators.models._

class JumpGame {

  def model: EnhancedModel = {

    val zero = LiteralInt(0)
    val one  = LiteralInt(1)

    val arr = ArgExpression(0, "arr", IntegerArrayType(), "j")  // j is the iterator
    //val index = ArgExpression(1, "i", IntegerType(), "i")
    val arr_length = ArrayLengthExpression(arr)
    //val eleAtIndex = ArrayElementExpression(arr, index)
    //val loopBound = AdditionExpression(eleAtIndex, index)
    val bound = List(arr)

    val i:HelperExpression = HelperExpression("i", one, SelfExpression("i") <= arr_length, arr_length + one)

    val result_min =
      MinRangeDefinition(
        "j",
        i +  one,
        SelfExpression("j") <= arr_length,
        SubproblemExpression(Seq(SelfExpression("j"))),
        SelfExpression("j") + one
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
      answer = ReturnExpressionDefinition(SubproblemExpression((Seq(zero))))
    )

    JumpGame
  }
}