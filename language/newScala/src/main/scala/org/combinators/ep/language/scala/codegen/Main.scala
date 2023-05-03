package org.combinators.ep.language.scala.codegen

/*DD:LD:AD*/

import cats.effect.{ExitCode, IO, IOApp}
import org.apache.commons.io.FileUtils
import org.combinators.ep.approach.oo._
import org.combinators.ep.domain.GenericModel
import org.combinators.ep.domain.abstractions.TestCase
import org.combinators.ep.domain.math._
import org.combinators.ep.generator.FileWithPathPersistable._
import org.combinators.ep.generator.{ApproachImplementationProvider, FileWithPath, FileWithPathPersistable, TestImplementationProvider}
import org.combinators.ep.language.inbetween.any.AbstractSyntax
import org.combinators.ep.language.inbetween.ffi.operatorExpression.FinalTypes
import org.combinators.jgitserv.{BranchTransaction, GitService}

import java.nio.file.{Path, Paths}

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 */
class Main {
  val generator = CodeGenerator()


  val ooApproach = Traditional[generator.syntax.type, generator.paradigm.type](generator.paradigm)(generator.nameProvider, generator.ooParadigm)
  // can't have all of these together
  /*val visitorApproach = Visitor[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  val visitorSideEffectApproach = Visitor.withSideEffects[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.imperativeInMethod, generator.ooParadigm)
  val extensibleVisitorApproach = ExtensibleVisitor[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  val interpreterApproach = Interpreter[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)

  val cocoCleanApproach = CoCoClean[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  val triviallyCleanApproach = TriviallyClean[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm)

  val dispatchApproach = RuntimeDispatch[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.imperativeInMethod, generator.stringsInMethod, generator.exceptionsInMethod, generator.ooParadigm)
  val algebraApproach = ObjectAlgebras[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)*/

  // these are all older versions of the cleaned-up Trivially and CoCo. Vita was intermediate result, kept around for historical accuracy
  //val vitaApproach = ViTA[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.imperativeInMethod, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  //val triviallyApproach = Trivially[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.imperativeInMethod, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  //val cocoApproach = CoCo[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.imperativeInMethod, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)

  // this one is not even working anymore. Needs serious work to get back to speed, and even then, it might not work past merging
  //val algebraApproach = Algebra[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.imperativeInMethod, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)

  // select one here.
  val approach = ooApproach// cocoCleanApproach//extensibleVisitorApproach

  val evolutions = Seq(M0, M1)//, M2, M3, M4, M5, M6, I1, I2, M7, M7I2)    // all test cases become active WHEN all included.
  //val evolutions = Seq(M0, M1, M2, M3, I1, A1, A1M3)

//  val eip = eips.I2(approach.paradigm)(generator.doublesInMethod, generator.realDoublesInMethod,
//    generator.stringsInMethod, generator.imperativeInMethod)
//  // how do I just use M2 instead of this? HACK
  val m0_eip = eips.M0(approach.paradigm)(generator.doubles,generator.strings)
  val m1_eip = eips.M1(approach.paradigm)(m0_eip)(generator.doubles)
  /*val m2_eip = eips.M2(approach.paradigm)(m1_eip)(generator.doublesInMethod, generator.stringsInMethod)

  //val m2_abs_eip = eips.M2_ABS(approach.paradigm)(m2_eip)(generator.doublesInMethod, generator.imperativeInMethod, generator.stringsInMethod)

  val m3_eip = eips.M3(approach.paradigm)(m2_eip)(generator.doublesInMethod, generator.stringsInMethod)

  val m4_eip = eips.M4.imperative[approach.paradigm.type,ApproachImplementationProvider.WithParadigm](approach.paradigm)(m3_eip)(
      generator.imperativeInMethod,
      generator.doublesInMethod,
      generator.booleansInMethod,
      generator.stringsInMethod,
      generator.listsInMethod,
      generator.equalityInMethod)
  val m5_eip = eips.M5(approach.paradigm)(m4_eip)(generator.intsInMethod,generator.treesInMethod)
  val m6_eip = eips.M6(approach.paradigm)(m5_eip)(generator.equalityInMethod, generator.booleansInMethod)
  val m7_eip = eips.M7(approach.paradigm)(m6_eip)(generator.doublesInMethod, generator.realDoublesInMethod, generator.stringsInMethod, generator.imperativeInMethod)
  val i1_eip = eips.I1(approach.paradigm)(m2_eip)(generator.doublesInMethod, generator.realDoublesInMethod, generator.stringsInMethod, generator.imperativeInMethod)
  val i2_eip = eips.I2(approach.paradigm)(i1_eip)(generator.doublesInMethod, generator.realDoublesInMethod, generator.stringsInMethod, generator.imperativeInMethod)
  val m7i2_eip = eips.M7I2.imperative[approach.paradigm.type,ApproachImplementationProvider.WithParadigm](approach.paradigm)(m7_eip,i2_eip)(
    generator.imperativeInMethod,
    generator.doublesInMethod,
    generator.booleansInMethod,
    generator.equalityInMethod)
  val m8_eip =  eips.M8.imperative[approach.paradigm.type,ApproachImplementationProvider.WithParadigm](approach.paradigm)(m7i2_eip)(
    generator.imperativeInMethod,
    generator.doublesInMethod,
    generator.booleansInMethod,
    generator.stringsInMethod,
    generator.equalityInMethod)
  val m9_eip = eips.M9(approach.paradigm)(m8_eip)(generator.doublesInMethod, generator.realDoublesInMethod, generator.imperativeInMethod)

  val a1_eip = eips.A1(approach.paradigm)(i1_eip)(generator.doublesInMethod, generator.stringsInMethod)
  val a1m3_eip = eips.A1M3(approach.paradigm)(m3_eip, a1_eip)(generator.stringsInMethod)

  //val eip = a1m3_eip*/
  val eip = m1_eip

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
            /*_ <- approach.implement(
              tests,
              TestImplementationProvider.defaultAssertionBasedTests(approach.paradigm)(generator.assertionsInMethod, generator.equalityInMethod, generator.booleansInMethod, generator.stringsInMethod)
            )*/
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
