package org.combinators.ep.language.java    /*DI:LD:AD*/

import cats.effect.{ExitCode, IO, IOApp}
import org.combinators.ep.approach.oo.{CoCoClean, ExtensibleVisitor, Interpreter, ObjectAlgebras, RuntimeDispatch, Traditional, TriviallyClean, Visitor}
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
class MainProducer(choice:String, select:String) {
  val generator = CodeGenerator(CodeGenerator.defaultConfig.copy(boxLevel = PartiallyBoxed))

  val ooApproach = Traditional[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm)
  val visitorApproach = Visitor[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  val extensibleVisitorApproach = ExtensibleVisitor[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  val visitorSideEffectApproach = Visitor.withSideEffects[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.imperativeInMethod, generator.ooParadigm)
  val interpreterApproach = Interpreter[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  val triviallyCleanApproach = TriviallyClean[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm)

  val dispatchApproach = RuntimeDispatch[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.imperativeInMethod, generator.stringsInMethod, generator.exceptionsInMethod, generator.ooParadigm)

  val cocoCleanApproach = CoCoClean[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  val algebraApproach = ObjectAlgebras[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)

  // select one here.
  //val approach = ooApproach // WORKS!
  // val approach = visitorApproach  // WORKS!
  // val approach = visitorSideEffectApproach // WORKS!
  // val approach = extensibleVisitorApproach // WORKS
  // val approach = triviallyApproach // WORKS!
  // val approach = vitaApproach // WORKS!
  // interpreterApproach NOT YET WORKING

  // select one here
  val approach = choice match {
    case "oo" => ooApproach
    case "visitor" => visitorApproach
    case "visitorSideEffect" => visitorSideEffectApproach
    case "extensibleVisitor" => extensibleVisitorApproach
    case "interpreter" => interpreterApproach
    case "coco" => cocoCleanApproach
    case "trivially" => triviallyCleanApproach
    case "dispatch" => dispatchApproach
    case "algebra" => algebraApproach

    case _ => ???
  }

  val evolutions = select match {
    case "M0" => Seq(M0)
    case "M1" => Seq(M0, M1)
    case "M2" => Seq(M0, M1, M2)
    case "M3" => Seq(M0, M1, M2, M3)
    case "W1" => Seq(M0, M1, W1)
    case "M3W1" => Seq(M0, M1, M2, M3, W1, M3W1)
    case "Q1" => Seq(M0, M1, M2, M3, W1, M3W1, Q1)
    case "C2" => Seq(M0, M1, M2, M3, W1, M3W1, Q1, C2)
    case "V1" => Seq(M0, M1, M2, M3, W1, M3W1, Q1, C2, V1)

    case _ => ???
  }

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
  val eip = select match {
    case "M0" => m0_eip
    case "M1" => m1_eip
    case "M2" => m2_eip
    case "M3" => m3_eip
    case "W1" => w1_eip
    case "M3W1" => m3w1_eip
    case "Q1" => q1_eip
    case "C2" => c2_eip
    case "V1" => v1_eip
    case _ => ???
  }
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

  def run(args: List[String]): IO[ExitCode] = {
    val approach = if (args.isEmpty) "algebra" else args.head
    val selection = if (args.isEmpty || args.tail.isEmpty) "W1" else args.tail.head
    new MainProducer(approach, selection).runGit(args)
  }
}

object DirectToDiskMainProducer extends IOApp {
  val targetDirectory = Paths.get("target", "ep3")

  def run(args: List[String]): IO[ExitCode] = {
    val approach = if (args.isEmpty) "algebra" else args.head
    val selection = if (args.isEmpty || args.tail.isEmpty) "V1" else args.tail.head

    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new MainProducer(approach, selection) }
      _ <- IO { println("[OK]") }
      result <- main.runDirectToDisc(targetDirectory)
    } yield result
  }
}
