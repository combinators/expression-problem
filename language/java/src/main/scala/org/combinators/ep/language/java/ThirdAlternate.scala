package org.combinators.ep.language.java

import cats.effect.{ExitCode, IO, IOApp}
import org.combinators.ep.approach.oo.{Trivially, ViTA}
import org.combinators.ep.domain.GenericModel
import org.combinators.ep.domain.abstractions.TestCase
import org.combinators.ep.domain.math.{M0, X1, X2, X2X3, X3, X4, eips}
import org.combinators.ep.generator.TestImplementationProvider
import org.combinators.jgitserv.{BranchTransaction, GitService}
import org.combinators.ep.generator.FileWithPathPersistable._
import org.combinators.ep.language.java.Main.generator

object ThirdAlternate extends IOApp {
  val generator = CodeGenerator(CodeGenerator.defaultConfig.copy(boxLevel = PartiallyBoxed))

  val vitaApproach = ViTA[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.imperativeInMethod, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  val triviallyApproach = Trivially[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.imperativeInMethod, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)

  val approach = triviallyApproach

  //val evolutions = Seq(M0, M1, M2, I1, I2)    // , I2 //       M3, M4, M5, M6) // ) // , M4, M5, M6)
  val evolutions = Seq( M0, X1, X2, X3, X2X3, X4)

  val x1_eip = eips.X1(approach.paradigm)(generator.doublesInMethod, generator.realDoublesInMethod, generator.stringsInMethod, generator.imperativeInMethod)
  val x2_eip = eips.X2(approach.paradigm)(x1_eip)(generator.doublesInMethod, generator.stringsInMethod)
  val x3_eip = eips.X3(approach.paradigm)(x2_eip)(generator.doublesInMethod, generator.stringsInMethod)

  val x2x3_eip = eips.X2X3(approach.paradigm)(x2_eip, x3_eip)

  val x4_eip = eips.X4(approach.paradigm)(x2x3_eip)(generator.doublesInMethod, generator.stringsInMethod)

  val eip = x4_eip

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
