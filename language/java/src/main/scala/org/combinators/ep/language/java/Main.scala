package org.combinators.ep.language.java

import java.nio.file.Paths

import cats.effect.{ExitCode, IO, IOApp}
import org.combinators.ep.approach.oo
import org.combinators.ep.approach.oo.{ExtensibleVisitor, Traditional, Visitor}
import org.combinators.ep.domain.Model
import org.combinators.ep.domain.abstractions.TestCase
import org.combinators.ep.domain.math._
import org.combinators.ep.generator.Command.Generator
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider, TestImplementationProvider}
import org.combinators.jgitserv.{BranchTransaction, GitService}
import org.eclipse.jgit.api.Git
import org.combinators.ep.approach.oo.Interpreter
import org.combinators.ep.generator.FileWithPathPersistable._

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 */
object Main extends IOApp {
  val generator = CodeGenerator(CodeGenerator.defaultConfig.copy(boxLevel = CodeGenerator.PartiallyBoxed))

  // can't have both of these?!
  val extensibleVisitorApproach = ExtensibleVisitor[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  val visitorApproach = Visitor[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  val ooApproach = Traditional[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm)
  val interpreterApproach = Interpreter[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)

  // select one here.
  val approach = ooApproach //extensibleVisitorApproach

  //val git = Git.init().setDirectory(directory.toFile).call()
  val evolutions = Seq(M0, M1, M2, M3)
  val tests = evolutions.scanLeft(Map.empty[Model, Seq[TestCase]]) { case (m, evolution) =>
    m + (evolution.getModel -> evolution.tests)
  }.tail

  // I would love to be able to debug a partially contructed model, and I thought there
  // should be some way to call out to this Main class to do this...
  //def debugResolve(val impl:Generator[ProjectContext, Unit]) : String = {
  //  generator.paradigm.runGenerator(impl).foreach(file => new String(file.rawBytes))
  //}

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

  def run(args: List[String]): IO[ExitCode] = {
    val name = evolutions.head.getModel.base.name
    for {
      _ <- IO { System.out.println(s"Use: git clone http://127.0.0.1:8081/$name") }
      exitCode <- new GitService(transaction.toSeq, name).run(args)
    } yield exitCode
  }
}
