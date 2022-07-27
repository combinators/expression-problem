package org.combinators.ep.language.java    /*DI:LD:AD*/

import cats.effect.{ExitCode, IO, IOApp}
import org.combinators.ep.approach.oo.{CoCoClean, ExtensibleVisitor, Interpreter, Traditional, Visitor}
import org.combinators.ep.domain.{GenericModel, Model}
import org.combinators.ep.domain.abstractions.TestCase
import org.combinators.ep.domain.math._
import org.combinators.ep.generator.{ApproachImplementationProvider, FileWithPath, FileWithPathPersistable, TestImplementationProvider}
import org.combinators.jgitserv.{BranchTransaction, GitService, ResourcePersistable}
import FileWithPathPersistable._
import org.apache.commons.io.{FileSystemUtils, FileUtils}

import java.nio.file.{Files, Path, Paths}

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 */
class MainProducer {
  val generator = CodeGenerator(CodeGenerator.defaultConfig.copy(boxLevel = PartiallyBoxed))

  val ooApproach = Traditional[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm)
  // can't have all of these together
  val visitorApproach = Visitor[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  val extensibleVisitorApproach = ExtensibleVisitor[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)

  val cocoCleanApproach = CoCoClean[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)

  // select one here.
  //val approach = ooApproach // WORKS!
  // val approach = visitorApproach  // WORKS!
  // val approach = visitorSideEffectApproach // WORKS!
  // val approach = extensibleVisitorApproach // WORKS
  // val approach = triviallyApproach // WORKS!
  // val approach = vitaApproach // WORKS!
  // interpreterApproach NOT YET WORKING
  val approach = cocoCleanApproach

  val evolutions = Seq(M0, M1, M2, M3, W1, M3W1, Q1, C2, V1)    // all test cases become active WHEN all included.

  //val evolutions = Seq(M0, M1, M2, M3, M4, M5, M6, M7) //
//  val eip = eips.I2(approach.paradigm)(generator.doublesInMethod, generator.realDoublesInMethod,
//    generator.stringsInMethod, generator.imperativeInMethod)
//  // how do I just use M2 instead of this? HACK
  val m0_eip = eips.M0(approach.paradigm)(generator.doublesInMethod, generator.stringsInMethod)
  val m1_eip = eips.M1(approach.paradigm)(m0_eip)(generator.doublesInMethod)
  val m2_eip = eips.M2(approach.paradigm)(m1_eip)(generator.doublesInMethod, generator.stringsInMethod)

  //val m2_abs_eip = eips.M2_ABS(approach.paradigm)(m2_eip)(generator.doublesInMethod, generator.imperativeInMethod, generator.stringsInMethod)

  val m3_eip = eips.M3(approach.paradigm)(m2_eip)(generator.doublesInMethod, generator.stringsInMethod)

  val w1_eip = eips.W1(approach.paradigm)(m1_eip)(generator.doublesInMethod, generator.realDoublesInMethod, generator.stringsInMethod)

  val m3w1_eip = eips.M3W1.imperative[approach.paradigm.type,ApproachImplementationProvider.WithParadigm](approach.paradigm)(m3_eip,w1_eip)(
    generator.imperativeInMethod,
    generator.doublesInMethod,
    generator.booleansInMethod,
    generator.equalityInMethod,
    generator.stringsInMethod
  )
  val q1_eip =  eips.Q1(approach.paradigm)(m3w1_eip)(
    generator.intsInMethod,
    generator.realDoublesInMethod,
    generator.treesInMethod,
    generator.stringsInMethod)
  val c2_eip = eips.C2.imperative[approach.paradigm.type,ApproachImplementationProvider.WithParadigm](approach.paradigm)(q1_eip)(
    generator.imperativeInMethod,
    generator.doublesInMethod,
    generator.booleansInMethod,
    generator.stringsInMethod,
    generator.listsInMethod,
    generator.equalityInMethod)

  //val eip = m7_eip
  val v1_eip = eips.V1.imperative[approach.paradigm.type,ApproachImplementationProvider.WithParadigm](approach.paradigm)(c2_eip)(
    generator.imperativeInMethod,
    generator.doublesInMethod,
    generator.booleansInMethod,
    generator.stringsInMethod,
    generator.equalityInMethod)

  //val eip = a1m3_eip
  val eip = v1_eip

  val tests = evolutions.scanLeft(Map.empty[GenericModel, Seq[TestCase]]) { case (m, evolution) =>
    m + (evolution.getModel -> evolution.tests)
  }.tail

  // for CoCo, we only need the latest since all earlier ones are identical
  val all = evolutions.zip(tests)

  def transaction[T](initialTransaction: T, addToTransaction: (T, String, () => Seq[FileWithPath]) => T): T = {
    all.foldLeft(initialTransaction) { case (transaction, (evolution, tests)) =>
      val impl =
        () => generator.paradigm.runGenerator {
          for {
            _ <- approach.implement(evolution.getModel, eip)
            _ <- approach.implement(
              tests,
              TestImplementationProvider.defaultAssertionBasedTests(approach.paradigm)(generator.assertionsInMethod, generator.equalityInMethod, generator.booleansInMethod, generator.stringsInMethod)
            )
          } yield ()
        }
      addToTransaction(transaction, evolution.getModel.name, impl)
    }
  }

  val persistable = FileWithPathPersistable[FileWithPath]

  def gitTransaction: Option[BranchTransaction] =
    transaction[Option[BranchTransaction]](Option.empty, (transaction, evolutionName, files) => {
      val nextTransaction =
        transaction.map(_.fork(evolutionName).deleteAllFiles)
          .getOrElse(BranchTransaction.empty(evolutionName))
      Some(nextTransaction.persist(files())(persistable).commit("Adding next evolution"))
    })

  def directToDiskTransaction(targetDirectory: Path): IO[Unit] = {
    transaction[IO[Unit]](IO.unit, (transaction, evolutionName, files) => IO {
      print("Computing Files...")
      val computed = files()
      println("[OK]")
      if (targetDirectory.toFile.exists()) {
        print(s"Cleaning Target Directory (${targetDirectory})...")
        FileUtils.deleteDirectory(targetDirectory.toFile)
        println("[OK]")
      }
      print("Persisting Files...")
      files().foreach(file => persistable.persistOverwriting(targetDirectory, file))
      println("[OK]")
    })
  }

  def runGit(args: List[String]): IO[ExitCode] = {
    val name = evolutions.head.getModel.base.name
    for {
      _ <- IO { System.out.println(s"Use: git clone http://127.0.0.1:8081/$name ${evolutions.last.getModel.name}") }
      exitCode <- new GitService(gitTransaction.toSeq, name).run(args)
      //exitCode <- new GitService(transaction.toSeq, name).runProcess(Seq(s"sbt", "test"))
    } yield exitCode
  }

  def runDirectToDisc(targetDirectory: Path): IO[ExitCode] = {
    for {
      _ <- directToDiskTransaction(targetDirectory)
    } yield ExitCode.Success
  }
}

object GitMainProducer extends IOApp {
  def run(args: List[String]): IO[ExitCode] = new MainProducer().runGit(args)
}

object DirectToDiskMainProducer extends IOApp {
  val targetDirectory = Paths.get("target", "ep2")

  def run(args: List[String]): IO[ExitCode] = {
    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new MainProducer() }
      _ <- IO { println("[OK]") }
      result <- main.runDirectToDisc(targetDirectory)
    } yield result
  }
}
