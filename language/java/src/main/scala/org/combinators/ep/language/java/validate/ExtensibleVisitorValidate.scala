package org.combinators.ep.language.java.validate     /*DD:LD:AD*/

import cats.effect.IOApp
import org.combinators.ep.approach.oo._
import org.combinators.ep.domain.math._
import org.combinators.ep.generator.FileWithPathPersistable._
import org.combinators.ep.generator.{ApproachImplementationProvider, TestImplementationProvider}
import org.combinators.ep.language.java.{JavaNameProvider, Syntax}
import org.combinators.jgitserv.BranchTransaction

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 */
object ExtensibleVisitorValidate extends IOApp with BaseEvolution {

  val approach = ExtensibleVisitor[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)

  // Massive one-liner, but this aligns with M8
  val eip = eips.M8.imperative[approach.paradigm.type,ApproachImplementationProvider.WithParadigm](approach.paradigm)(eips.M7I2.imperative[approach.paradigm.type,ApproachImplementationProvider.WithParadigm](approach.paradigm)(eips.M7(approach.paradigm)(eips.M6(approach.paradigm)(eips.M5(approach.paradigm)(eips.M4.imperative[approach.paradigm.type,ApproachImplementationProvider.WithParadigm](approach.paradigm)(eips.M3(approach.paradigm)(eips.M2(approach.paradigm)(eips.M1(approach.paradigm)(eips.M0(approach.paradigm)(generator.doublesInMethod))(generator.doublesInMethod))(generator.doublesInMethod, generator.stringsInMethod))(generator.doublesInMethod, generator.stringsInMethod))(generator.imperativeInMethod,generator.doublesInMethod,generator.booleansInMethod,generator.stringsInMethod,generator.listsInMethod,generator.equalityInMethod))(generator.intsInMethod,generator.treesInMethod))(generator.equalityInMethod))(generator.doublesInMethod, generator.realDoublesInMethod, generator.stringsInMethod, generator.imperativeInMethod),eips.I2(approach.paradigm)(eips.I1(approach.paradigm)(eips.M2(approach.paradigm)(eips.M1(approach.paradigm)(eips.M0(approach.paradigm)(generator.doublesInMethod))(generator.doublesInMethod))(generator.doublesInMethod, generator.stringsInMethod))(generator.doublesInMethod, generator.realDoublesInMethod, generator.stringsInMethod, generator.imperativeInMethod))(generator.doublesInMethod, generator.realDoublesInMethod, generator.stringsInMethod, generator.imperativeInMethod))(generator.imperativeInMethod,generator.doublesInMethod,generator.booleansInMethod,generator.equalityInMethod))(generator.imperativeInMethod,generator.doublesInMethod,generator.booleansInMethod,generator.stringsInMethod,generator.equalityInMethod)

  // would love to be above to lift this up to BaseEvolution...
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
