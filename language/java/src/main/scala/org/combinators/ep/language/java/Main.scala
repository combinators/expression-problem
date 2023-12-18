package org.combinators.ep.language.java     /*DD:LD:AD*/

import cats.effect.{ExitCode, IO, IOApp}
import org.combinators.ep.approach.oo.{CoCoClean, ExtensibleVisitor, Interpreter, ObjectAlgebras, RuntimeDispatch, Traditional, TriviallyClean, Visitor}
import org.combinators.ep.domain.GenericModel
import org.combinators.ep.domain.abstractions.TestCase
import org.combinators.ep.domain.math._
import org.combinators.ep.generator.{ApproachImplementationProvider, FileWithPath, FileWithPathPersistable, TestImplementationProvider}
import org.combinators.jgitserv.{BranchTransaction, GitService}
import FileWithPathPersistable._
import org.apache.commons.io.FileUtils

import org.combinators.ep.domain.math.{M0, eips}
import org.combinators.ep.domain.math.systemI.{I1, I2}
import org.combinators.ep.generator.{FileWithPath, FileWithPathPersistable, TestImplementationProvider}
import org.combinators.jgitserv.{BranchTransaction, GitService}
import org.combinators.ep.generator.FileWithPathPersistable._

import java.nio.file.{Path, Paths}

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 */
class Main(choice:String, select:String) {
  val generator = CodeGenerator(CodeGenerator.defaultConfig.copy(boxLevel = PartiallyBoxed))

  val ooApproach = Traditional[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm)
  // can't have all of these together
  val visitorApproach = Visitor[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  val visitorSideEffectApproach = Visitor.withSideEffects[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.imperativeInMethod, generator.ooParadigm)
  val extensibleVisitorApproach = ExtensibleVisitor[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  val interpreterApproach = Interpreter[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)

  val cocoCleanApproach = CoCoClean[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  val triviallyCleanApproach = TriviallyClean[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm)

  val dispatchApproach = RuntimeDispatch[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.imperativeInMethod, generator.stringsInMethod, generator.exceptionsInMethod, generator.ooParadigm)
  val algebraApproach = ObjectAlgebras[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)

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
    case "M4" => Seq(M0, M1, M2, M3, M4)
    case "M5" => Seq(M0, M1, M2, M3, M4, M5)
    case "M6" => Seq(M0, M1, M2, M3, M4, M5, M6)
    case "M7" => Seq(M0, M1, M2, M3, M4, M5, M7)
    case "M7I2" => Seq(M0, M1, M2, M3, M4, M5, M7, I1, I2, M7I2)
    case "M8" => Seq(M0, M1, M2, M3, M4, M5, M7, I1, I2, M7I2, M8)
    case "M9" => Seq(M0, M1, M2, M3, M4, M5, M7, I1, I2, M7I2, M8, M9)

    case "I1" => Seq(M0, M1, M2, I1)
    case "A1" => Seq(M0, M1, M2, I1, A1)
    case "A1M3" => Seq(M0, M1, M2, M3, I1, A1, A1M3)
    case "A1M3I2" => Seq(M0, M1, M2, M3, I1, A1, A1M3, I2, A1M3I2)
    case "A3" => Seq(M0, M1, M2, M3, I1, A1, A1M3, I2, A1M3I2, A3)

    case "I2" => Seq(M0, M1, M2, I1, I2)

    case _ => ???
  }

  //  val eip = eips.I2(approach.paradigm)(generator.doublesInMethod, generator.realDoublesInMethod,
//    generator.stringsInMethod, generator.imperativeInMethod)
//  // how do I just use M2 instead of this? HACK
  val m0_eip = eips.M0(approach.paradigm)(generator.doublesInMethod,generator.stringsInMethod)
  val m1_eip = eips.M1(approach.paradigm)(m0_eip)(generator.doublesInMethod)
  val m2_eip = eips.M2(approach.paradigm)(m1_eip)(generator.doublesInMethod, generator.stringsInMethod)

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
  val i1_eip = eips.systemI.I1(approach.paradigm)(m2_eip)(generator.doublesInMethod, generator.realDoublesInMethod, generator.stringsInMethod, generator.imperativeInMethod)
  val i2_eip = eips.systemI.I2(approach.paradigm)(i1_eip)(generator.doublesInMethod, generator.realDoublesInMethod, generator.stringsInMethod, generator.imperativeInMethod)
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
  val a1m3i2_eip = eips.A1M3I2(approach.paradigm)(a1m3_eip, i2_eip)(generator.stringsInMethod)
  val a3_eip = eips.A3(approach.paradigm)(a1m3i2_eip)(generator.doublesInMethod, generator.stringsInMethod)

  //val eip = a1_eip// a1m3i2_eip// a1m3_eip   //  a3_eip
  //val eip = m8_eip//m8_eip //m7i2_eip// m8_eip

  val eip = select match {
    case "M0" => m0_eip
    case "M1" => m1_eip
    case "M2" => m2_eip
    case "M3" => m3_eip
    case "M4" => m4_eip
    case "M5" => m5_eip
    case "M6" => m6_eip
    case "M7" => m7_eip
    case "M7I2" => m7i2_eip
    case "M8" => m8_eip
    case "M9" => m9_eip

    case "I1" => i1_eip
    case "A1" => a1_eip
    case "A1M3" => a1m3_eip
    case "A1M3I2" => a1m3i2_eip
    case "A3" => a3_eip

    case "I2" => i2_eip

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

object GitMain extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {
    val approach = if (args.isEmpty) "algebra" else args.head
    val selection = if (args.isEmpty || args.tail.isEmpty) "M7I2" else args.tail.head
    new Main(approach, selection).runGit(args)
  }
}

object DirectToDiskMain extends IOApp {
  val targetDirectory = Paths.get("target", "ep3")

  def run(args: List[String]): IO[ExitCode] = {
    val approach = if (args.isEmpty) "interpreter" else args.head
    if (approach == "exit") { sys.exit(0) }
    val selection = if (args.isEmpty || args.tail.isEmpty) "M8" else args.tail.head
    // A1M3 fails for interpreter
    // A1M3I2 generates for all, fails to compile in interpreter
    println("Generating " + approach + " for " + selection)
    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new Main(approach, selection) }
      _ <- IO { println("[OK]") }
      result <- main.runDirectToDisc(targetDirectory)
    } yield result
  }
}
