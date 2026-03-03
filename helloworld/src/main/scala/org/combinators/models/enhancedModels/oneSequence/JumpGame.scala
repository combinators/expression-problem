package org.combinators.models.enhancedModels.oneSequence

import org.combinators.models._

class JumpGame {

  def model: EnhancedModel = {

    val zero = new LiteralInt(0)
    val one  = new LiteralInt(1)

    val arr = new ArgExpression(0, "arr", IntegerArrayType(), "arr")
    val index = new ArgExpression(1, "i", IntegerType(), "i")
    val arr_length = new ArrayLengthExpression(arr)
    val eleAtIndex = new ArrayElementExpression(arr, index)
    val loopBound = new AdditionExpression(eleAtIndex, index)
    val bound = List(arr, index)

    val j:HelperExpression = HelperExpression("j", index+one, SelfExpression("j") <= loopBound, loopBound + one)

    val recursiveCall = new SubproblemExpression(Seq(j))

    val result_min =
      MinRangeDefinition(
        "j",
        new AdditionExpression(index, one),
        SelfExpression("j") <= loopBound,
        recursiveCall,
        new AdditionExpression(SelfExpression("j"), one)
      )

    val helperTable = Map("j" -> j)

    val sol_dt = SubproblemInvocation(Seq("j"), helpers = helperTable)

    val basecase = IfThenElseDefinition(arr_length - one <= index, ExpressionStatement(zero), result_min)

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