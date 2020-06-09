package org.combinators.ep.language.java.validate     /*DD:LD:AD*/

import cats.effect.IOApp
import org.combinators.ep.approach.oo._
import org.combinators.ep.domain.math._
import org.combinators.ep.generator.TestImplementationProvider
import org.combinators.ep.generator.FileWithPathPersistable._
import org.combinators.ep.language.java.{JavaNameProvider, Syntax}
import org.combinators.jgitserv.BranchTransaction

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 */
object TraditionalValidate extends IOApp with BaseEvolution {

  val approach = Traditional[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm)

  // Massive one-liner, but this aligns with M7I2
  val eip = eips.M7I2(approach.paradigm)(eips.M7(approach.paradigm)(eips.M6(approach.paradigm)(eips.M5(approach.paradigm)(eips.M4.imperative(approach.paradigm)(generator.imperativeInMethod,generator.doublesInMethod,generator.booleansInMethod,generator.stringsInMethod,generator.listsInMethod,generator.equalityInMethod))(generator.intsInMethod,generator.treesInMethod))(generator.equalityInMethod))(generator.doublesInMethod, generator.realDoublesInMethod, generator.stringsInMethod, generator.imperativeInMethod))  (eips.I2(approach.paradigm)(generator.doublesInMethod, generator.realDoublesInMethod, generator.stringsInMethod, generator.imperativeInMethod))

  val transaction =
    evolutions.zip(tests).foldLeft(Option.empty[BranchTransaction]) {
      case (transaction, (evolution, tests)) =>
        val impl =
          for {
            _ <- approach.implement(evolution.getModel,eip)
            _ <- approach.implement(
              tests,
              TestImplementationProvider.defaultAssertionBasedTests(approach.paradigm)(generator.assertionsInMethod, generator.equalityInMethod, generator.booleansInMethod, generator.stringsInMethod)
            )
          } yield ()
        val nextTransaction =
          transaction.map(_.fork(evolution.getModel.name).deleteAllFiles)
            .getOrElse(BranchTransaction.empty(evolution.getModel.name))
        Some(nextTransaction.persist(generator.paradigm.runGenerator(impl)).commit("Adding next evolution"))
    }
}
