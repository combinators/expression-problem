package org.combinators.ep.language.java.validate     /*DD:LD:AD*/

import cats.effect.IOApp
import org.combinators.ep.approach.oo._
import org.combinators.ep.domain.math._
import org.combinators.ep.generator.{ApproachImplementationProvider, TestImplementationProvider}
import org.combinators.ep.generator.FileWithPathPersistable._
import org.combinators.ep.language.java.validate.ExtensibleVisitorValidate.{approach, generator}
import org.combinators.ep.language.java.{JavaNameProvider, Syntax}
import org.combinators.jgitserv.BranchTransaction

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 */
object TraditionalValidate extends IOApp with BaseEvolution {

  val approach = Traditional[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm)

  // Massive one-liner, but this aligns with M7I2

  // Massive one-liner, but this aligns with M7I2
  val m0eip = eips.M0(approach.paradigm)(generator.doublesInMethod)
  val m1eip = eips.M1(approach.paradigm)(m0eip)(generator.doublesInMethod)
  val m2eip = eips.M2(approach.paradigm)(m1eip)(generator.doublesInMethod, generator.stringsInMethod)
  val m3eip = eips.M3(approach.paradigm)(m2eip)(generator.doublesInMethod, generator.stringsInMethod)
  val m4eip = eips.M4.imperative[approach.paradigm.type,ApproachImplementationProvider.WithParadigm](approach.paradigm)(m3eip)(
    generator.imperativeInMethod,
    generator.doublesInMethod,
    generator.booleansInMethod,
    generator.stringsInMethod,
    generator.listsInMethod,
    generator.equalityInMethod)
  val m5eip = eips.M5(approach.paradigm)(m4eip)(generator.intsInMethod,generator.treesInMethod)
  val m6eip = eips.M6(approach.paradigm)(m5eip)(generator.equalityInMethod)
  val e7_eip = eips.M7(approach.paradigm)(m6eip)(generator.doublesInMethod, generator.realDoublesInMethod, generator.stringsInMethod, generator.imperativeInMethod)
  val i1_eip = eips.I1(approach.paradigm)(m2eip)(generator.doublesInMethod, generator.realDoublesInMethod, generator.stringsInMethod, generator.imperativeInMethod)
  val i2_eip = eips.I2(approach.paradigm)(m1eip)(generator.doublesInMethod, generator.realDoublesInMethod, generator.stringsInMethod, generator.imperativeInMethod)
  val m7i2_eip = eips.M7I2(approach.paradigm)(e7_eip)(i2_eip)
  val m8_eip =  eips.M8.imperative[approach.paradigm.type,ApproachImplementationProvider.WithParadigm](approach.paradigm)(m7i2_eip)(
    generator.imperativeInMethod,
    generator.doublesInMethod,
    generator.booleansInMethod,
    generator.stringsInMethod,
    generator.equalityInMethod)
  val eip = m7i2_eip

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
