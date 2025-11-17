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

  def make_solution(len1: Expression, len2: Expression, dp: Expression, r: Expression, c: Expression, optimizationBody: Statement): Generator[paradigm.MethodBodyContext, Option[Expression]] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._

    // assumes that the base cases are the same for both sequences
    def make_base_cases(rowVar: Expression, rowGuard: Expression, rowUpdate: Expression,
                        colVar: Expression, colGuard: Expression, colUpdate: Expression,
                        relation: Expression): Generator[MethodBodyContext, Seq[Statement]] = {

      for {
        zero <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 0)

        dp_row_0 <- get_matrix_element(dp, rowVar, zero)
        row_relation <- impParadigm.imperativeCapabilities.assignVar(dp_row_0, relation)

        dp_0_col <- get_matrix_element(dp, zero, colVar)
        col_relation <- impParadigm.imperativeCapabilities.assignVar(dp_0_col, relation)

        len1_base_case <- make_for_loop(rowVar, rowGuard, rowUpdate, Seq(row_relation))
        len2_base_case <- make_for_loop(colVar, colGuard, colUpdate, Seq(col_relation))
      } yield Seq(len1_base_case, len2_base_case)
    }

    def make_optimization_step(guard1: Expression, update1: Expression,
                               guard2: Expression, update2: Expression): Generator[MethodBodyContext, Statement] = {

      for {
        while_loop <- make_nested_for_loop(r, guard1, update1, c, guard2, update2, Seq(optimizationBody), Seq.empty)
      } yield while_loop
    }

    for {
      stringType <- toTargetLanguageType(TypeRep.String)
      intType <- toTargetLanguageType(TypeRep.Int)
      arrayType <- toTargetLanguageType(TypeRep.Array(TypeRep.Int))
      array2dType <- toTargetLanguageType(TypeRep.Array(TypeRep.Array(TypeRep.Int)))
      one <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 1)
      zero <- paradigm.methodBodyCapabilities.reify(TypeRep.Int, 0)

      // base cases
      // todo: figure out how to differentiate between integer and string to use the correct length method/attribute
      rBaseCase <- declare_and_inst_variable("rBaseCase", intType, zero)
      rBaseCase_guard <- arithmetic.arithmeticCapabilities.le(rBaseCase, len1)
      rBaseCase_update <- arithmetic.arithmeticCapabilities.add(rBaseCase, one)

      cBaseCase <- declare_and_inst_variable("cBaseCase", intType, zero)
      cBaseCase_guard <- arithmetic.arithmeticCapabilities.le(cBaseCase, len2)
      cBaseCase_update <- arithmetic.arithmeticCapabilities.add(cBaseCase, one)

      test_stmt <- impParadigm.imperativeCapabilities.assignVar(rBaseCase, zero)

      base_cases <- make_base_cases(rBaseCase, rBaseCase_guard, rBaseCase_update, cBaseCase, cBaseCase_guard, cBaseCase_update, zero)
      _ <- addBlockDefinitions(base_cases)

      // optimization step
      r_guard <- arithmetic.arithmeticCapabilities.lt(r, len1)
      r_update <- arithmetic.arithmeticCapabilities.add(r, one)

      c_guard <- arithmetic.arithmeticCapabilities.lt(c, len2)
      c_update <- arithmetic.arithmeticCapabilities.add(c, one)

      optimization_step <- make_optimization_step(r_guard, r_update, c_guard, c_update)
      _ <- addBlockDefinitions(Seq(optimization_step))

      return_stmt <- get_bottom_right_dp_element(dp, len1, len2)
    } yield Option(return_stmt)
  }

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
