package org.combinators.ep.language.scala     /*DI:LD:AD*/

import cats.effect.{ExitCode, IO, IOApp}
import org.apache.commons.io.FileUtils
import org.combinators.ep.domain.GenericModel
import org.combinators.ep.approach.functional.Traditional
import org.combinators.ep.domain.abstractions.TestCase
import org.combinators.ep.domain.math._
import org.combinators.ep.generator.{ApproachImplementationProvider, FileWithPath, FileWithPathPersistable, TestImplementationProvider}
import org.combinators.jgitserv.{BranchTransaction, GitService}
import org.combinators.ep.generator.FileWithPathPersistable._

import java.nio.file.{Path, Paths}

/**
  * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
  */
class Main  {
  val generator = CodeGenerator(CodeGenerator.defaultConfig)

  val traditionalApproach = Traditional[Syntax.default.type, generator.paradigm.type](generator.paradigm)(ScalaNameProvider, generator.functional, generator.functionalInMethod)

 /*val visitorApproach = Visitor[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  val visitorSideEffectApproach = VisitorSideEffect[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.imperativeInMethod, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  val extensibleVisitorApproach = ExtensibleVisitor[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  val interpreterApproach = Interpreter[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.imperativeInMethod, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  val triviallyApproach = Trivially[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.imperativeInMethod, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  val algebraApproach = Algebra[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.imperativeInMethod, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  */

  // select one here.
  // val approach = ooApproach // TODO!
  // val approach = visitorApproach  // TODO!
  // val approach = visitorSideEffectApproach // TODO!
  // val approach = extensibleVisitorApproach // TODO!
  // val approach = triviallyApproach // TODO!
  // val approach = algebraApproach // TODO!
  val approach = traditionalApproach

  val evolutions = Seq(M0, M1, M2, M3, M4)

  val m0_eip = eips.M0(approach.paradigm)(generator.doublesInMethod)
  val m1_eip = eips.M1(approach.paradigm)(m0_eip)(generator.doublesInMethod)
  val m2_eip = eips.M2(approach.paradigm)(m1_eip)(generator.doublesInMethod, generator.stringsInMethod)
  val m3_eip = eips.M3(approach.paradigm)(m2_eip)(generator.doublesInMethod, generator.stringsInMethod)
  val m4_eip = eips.M4.functional[approach.paradigm.type,ApproachImplementationProvider.WithParadigm](approach.paradigm)(m3_eip)(
    generator.functionalInMethod,
    generator.doublesInMethod,
    generator.booleansInMethod,
    generator.stringsInMethod,
    generator.listsInMethod,
    generator.equalityInMethod)

  val eip = m4_eip
//   val eip =
//    eips.M4.functional(approach.paradigm)(
//      generator.functionalInMethod,
//      generator.doublesInMethod,
//      generator.booleansInMethod,
//      generator.stringsInMethod,
//      generator.listsInMethod,
//      generator.equalityInMethod)
  /*val m5eip = eips.M5(approach.paradigm)(m4eip)(
    generator.intsInMethod,
    generator.treesInMethod) // TODO: implement trees
  val eip = eips.M6(approach.paradigm)(m5eip)(
    generator.equalityInMethod
  )*/

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

object GitMain extends IOApp {
  def run(args: List[String]): IO[ExitCode] = new Main().runGit(args)
}

object DirectToDiskMain extends IOApp {
  val targetDirectory = Paths.get("target", "ep2")

  def run(args: List[String]): IO[ExitCode] = {
    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new Main() }
      _ <- IO { println("[OK]") }
      result <- main.runDirectToDisc(targetDirectory)
    } yield result
  }
}
