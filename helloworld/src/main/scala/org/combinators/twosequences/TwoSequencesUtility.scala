package org.combinators.twosequences

import org.combinators.ep.domain.abstractions.TypeRep
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.NameProvider
import org.combinators.ep.generator.paradigm.{AnyParadigm, ObjectOriented}
import org.combinators.ep.generator.paradigm.control.Imperative
import org.combinators.ep.generator.paradigm.ffi.{Arithmetic, Arrays, Assertions, Console, Equality}

import org.combinators.dp.Utility

trait TwoSequencesUtility extends Utility {
  import paradigm._
  import syntax._
  import ooParadigm._

  def instantiate_dp(numRows: Expression, numCols: Expression): Generator[MethodBodyContext, Expression] = {
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._

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
    import paradigm.methodBodyCapabilities._
    import ooParadigm.methodBodyCapabilities._

    for {
      dpBottomRight <- get_matrix_element(dp, len1, len2)
    } yield dpBottomRight
  }
}
