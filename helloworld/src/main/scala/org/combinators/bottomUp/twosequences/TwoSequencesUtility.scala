package org.combinators.bottomUp.twosequences

import org.combinators.dp.Utility
import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.generator.Command.Generator

trait TwoSequencesUtility extends Utility {
  import paradigm._
  import syntax._

  // test
//  def initialize_solution(val1: Expression, val2: Expression, domainType: Type): Seq[Generator[MethodBodyContext, Expression]] = {
//
//    for {
//      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)
//      zero <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 0)
//
//      len1Value <- ooParadigm.methodBodyCapabilities.getMember(val1, names.mangle("length"))
//      len2Value <- ooParadigm.methodBodyCapabilities.getMember(val2, names.mangle("length"))
//      len1 <- declare_and_inst_variable("len1", domainType, len1Value)
//      len2 <- declare_and_inst_variable("len2", domainType, len2Value)
//
//      len1PlusOne <- arithmetic.arithmeticCapabilities.add(len1, one)
//      len2PlusOne <- arithmetic.arithmeticCapabilities.add(len2, one)
//
//      dp <- instantiate_dp(len1PlusOne, len2PlusOne)
//
//      r <- declare_and_inst_variable("r", domainType, zero)
//      c <- declare_and_inst_variable("c", domainType, zero)
//    } yield Seq(dp, r, c, len1, len2)
//  }

  def instantiate_dp(numRows: Expression, numCols: Expression): Generator[MethodBodyContext, Expression] = {
    import paradigm.methodBodyCapabilities._

    for {
      array2dType <- toTargetLanguageType(TypeRep.Array(TypeRep.Array(TypeRep.Int)))

      instantiated <- ooParadigm.methodBodyCapabilities.instantiateObject(
        array2dType,
        Seq(numRows, numCols),
        None
      )

      dpVar <- declare_and_inst_variable("dp", array2dType, instantiated)
    } yield dpVar
  }

  def get_bottom_right_dp_element(dp: Expression, len1: Expression, len2: Expression): Generator[MethodBodyContext, Expression] = {

    for {
      dpBottomRight <- get_matrix_element(dp, len1, len2)
    } yield dpBottomRight
  }
}
