package org.combinators.ep.language.java     /*DD:LD:AD*/

import cats.effect.{ExitCode, IO, IOApp}
import org.combinators.ep.approach.oo.ViTA
import org.combinators.ep.domain.GenericModel
import org.combinators.ep.domain.abstractions.TestCase
import org.combinators.ep.domain.math.{A1, A1M3, A1M3I2, A3, I1, I2, M0, M1, M2, eips}
import org.combinators.ep.generator.TestImplementationProvider
import org.combinators.jgitserv.{BranchTransaction, GitService}
import org.combinators.ep.generator.FileWithPathPersistable._

object AlternateMain extends IOApp {
  val generator = CodeGenerator(CodeGenerator.defaultConfig.copy(boxLevel = PartiallyBoxed))

  val vitaApproach = ViTA[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.imperativeInMethod, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)

  val approach = vitaApproach

  val evolutions = Seq(M0, M1, M2, I1, I2, A1, A1M3, A1M3I2, A3)  // M0, M1, M2, M3, M4, M5, M6, M7, I1, I2,)    // all test cases become active WHEN all included.

  val m0eip = eips.M0(approach.paradigm)(generator.doublesInMethod)
  val m1eip = eips.M1(approach.paradigm)(m0eip)(generator.doublesInMethod)
  val m2eip = eips.M2(approach.paradigm)(m1eip)(generator.doublesInMethod, generator.stringsInMethod)
  val m3_eip = eips.M3(approach.paradigm)(m2eip)(generator.doublesInMethod, generator.stringsInMethod)

  val i1_eip = eips.I1(approach.paradigm)(m2eip)(generator.doublesInMethod, generator.realDoublesInMethod, generator.stringsInMethod, generator.imperativeInMethod)

  val i2_eip = eips.I2(approach.paradigm)(i1_eip)(generator.doublesInMethod, generator.realDoublesInMethod, generator.stringsInMethod, generator.imperativeInMethod)
  val a1_eip = eips.A1(approach.paradigm)(i1_eip)(generator.doublesInMethod, generator.stringsInMethod)

  val a1m3_eip = eips.A1M3(approach.paradigm)(m3_eip, a1_eip)(generator.stringsInMethod)

  val a1m3i2_eip = eips.A1M3I2(approach.paradigm)(a1m3_eip, i2_eip)(generator.stringsInMethod)

  val eip = eips.A3(approach.paradigm)(a1m3i2_eip)(generator.doublesInMethod, generator.stringsInMethod)

  val tests = evolutions.scanLeft(Map.empty[GenericModel, Seq[TestCase]]) { case (m, evolution) =>
    m + (evolution.getModel -> evolution.tests)
  }.tail

  val transaction =
    evolutions.zip(tests).foldLeft(Option.empty[BranchTransaction]) {
      case (transaction, (evolution, tests)) =>
        val impl =
          for {
            _ <- approach.implement(evolution.getModel, eip)
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

  def run(args: List[String]): IO[ExitCode] = {
    val name = evolutions.head.getModel.base.name
    for {
      _ <- IO { System.out.println(s"Use: git clone http://127.0.0.1:8081/$name ${evolutions.last.getModel.name}") }
      exitCode <- new GitService(transaction.toSeq, name).run(args)
      //exitCode <- new GitService(transaction.toSeq, name).runProcess(Seq(s"sbt", "test"))
    } yield exitCode
  }
}
