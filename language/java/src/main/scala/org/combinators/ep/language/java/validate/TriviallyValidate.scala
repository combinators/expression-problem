package org.combinators.ep.language.java.validate

import cats.effect.IOApp
import org.combinators.ep.approach.oo._
import org.combinators.ep.domain.math._
import org.combinators.ep.generator.TestImplementationProvider
import org.combinators.ep.generator.FileWithPathPersistable._
import org.combinators.ep.language.java.{JavaNameProvider, Syntax}
import org.combinators.jgitserv.BranchTransaction

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 *
 * TODO: HANGS FOR SOME REASON!
 */
object TriviallyValidate extends IOApp with BaseEvolution {
  val approach = Trivially[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)

  val transaction =
    evolutions.zip(tests).foldLeft(Option.empty[BranchTransaction]) {
      case (transaction, (evolution, tests)) =>
        val impl =
          for {
            _ <- approach.implement(
              evolution.getModel,
              eips.M3.apply(approach.paradigm)(generator.doublesInMethod, generator.stringsInMethod)
            )
            _ <- approach.implement(
              tests,
              TestImplementationProvider.defaultAssertionBasedTests(approach.paradigm)(generator.assertionsInMethod, generator.equalityInMethod, generator.booleansInMethod)
            )
          } yield ()
        val nextTransaction =
          transaction.map(_.fork(evolution.getModel.name).deleteAllFiles)
            .getOrElse(BranchTransaction.empty(evolution.getModel.name))
        Some(nextTransaction.persist(generator.paradigm.runGenerator(impl)).commit("Adding next evolution"))
    }
}
