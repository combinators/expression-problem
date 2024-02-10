package org.combinators.ep.language.java.systemJ     /*DD:LD:AD*/

import cats.effect.{ExitCode, IO, IOApp}
import org.apache.commons.io.FileUtils
import org.combinators.ep.approach.oo._
import org.combinators.ep.domain.GenericModel
import org.combinators.ep.domain.abstractions.TestCase
import org.combinators.ep.domain.math.{eips, _}
import org.combinators.ep.domain.math.systemJ.{J1, J2, J3, J4, J5, J6}
import org.combinators.ep.domain.math.systemJK.{J7, J8, K2J6}
import org.combinators.ep.domain.math.systemK.{K1, K2}
import org.combinators.ep.generator.FileWithPathPersistable._
import org.combinators.ep.generator.{ApproachImplementationProvider, FileWithPath, FileWithPathPersistable, TestImplementationProvider}
import org.combinators.ep.language.java.{CodeGenerator, JavaNameProvider, PartiallyBoxed, Syntax}
import org.combinators.jgitserv.BranchTransaction

import java.nio.file.{Path, Paths}

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 */
class MainJ(choice:String, select:String) {
  val generator = CodeGenerator(CodeGenerator.defaultConfig.copy(boxLevel = PartiallyBoxed))
  val visualizeApproach = Visualize[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm)

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
    case "graphviz" => visualizeApproach
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

  // select one here
  val evolutions = select match {
    case "M0" => Seq(M0)
    case "J1" => Seq(M0, J1)
    case "J2" => Seq(M0, J1, J2)
    case "J3" => Seq(M0, J1, J2, J3)
    case "K1" => Seq(M0, J1, J2, K1)
    case "K2" => Seq(M0, J1, J2, K1, K2)
    case "J4" => Seq(M0, J1, J2, J3, J4)
    case "J5" => Seq(M0, J1, J2, J3, J4, J5)
    case "J6" => Seq(M0, J1, J2, J3, J4, J5, J6)
    case "K2J6" => Seq(M0, J1, J2, K1, K2, J3, J4, J5, J6, K2J6)
    case "J7" => Seq(M0, J1, J2, K1, K2, J3, J4, J5, J6, K2J6, J7)
    case "J8" => Seq(M0, J1, J2, K1, K2, J3, J4, J5, J6, K2J6, J7, J8)
    case _ => ???
  }

    val m0_eip = eips.M0(approach.paradigm)(generator.doublesInMethod,generator.stringsInMethod)

    val j1_eip = eips.systemJ.J1(approach.paradigm)(m0_eip)(generator.doublesInMethod,generator.realDoublesInMethod,generator.stringsInMethod,generator.imperativeInMethod)
    val j2_eip = eips.systemJ.J2(approach.paradigm)(j1_eip)(generator.doublesInMethod,generator.booleansInMethod,generator.equalityInMethod)
    val j3_eip = eips.systemJ.J3(approach.paradigm)(j2_eip)(generator.doublesInMethod,generator.booleansInMethod,generator.stringsInMethod)
    val k1_eip = eips.systemK.K1(approach.paradigm)(j2_eip)(generator.doublesInMethod,generator.realDoublesInMethod,generator.booleansInMethod,generator.stringsInMethod,generator.imperativeInMethod)
    val j4_eip = eips.systemJ.J4(approach.paradigm)(j3_eip)(generator.intsInMethod,generator.treesInMethod)
    val j5_eip = eips.systemJ.J5(approach.paradigm)(j4_eip)(generator.equalityInMethod,generator.booleansInMethod)
    val j6_eip = eips.systemJ.J6(approach.paradigm)(j5_eip)(generator.doublesInMethod,generator.realDoublesInMethod,generator.stringsInMethod,generator.imperativeInMethod)

    val k2_eip = eips.systemK.K2.imperative[approach.paradigm.type,ApproachImplementationProvider.WithParadigm](approach.paradigm)(k1_eip)(
      generator.imperativeInMethod,
      generator.doublesInMethod,
      generator.booleansInMethod,
      generator.stringsInMethod,
      generator.listsInMethod,
      generator.equalityInMethod)

    val k2j6_eip = eips.systemJK.K2J6.imperative[approach.paradigm.type,ApproachImplementationProvider.WithParadigm](approach.paradigm)(j6_eip,k2_eip)(
      generator.imperativeInMethod,
      generator.doublesInMethod,
      generator.booleansInMethod,
      generator.stringsInMethod,
      generator.equalityInMethod)

    val j7_eip =  eips.systemJK.J7.imperative[approach.paradigm.type,ApproachImplementationProvider.WithParadigm](approach.paradigm)(k2j6_eip)(
      generator.imperativeInMethod,
      generator.doublesInMethod,
      generator.booleansInMethod,
      generator.stringsInMethod,
      generator.equalityInMethod)
    val j8_eip = eips.systemJK.J8(approach.paradigm)(j7_eip)(generator.doublesInMethod, generator.realDoublesInMethod, generator.imperativeInMethod)

  val eip = select match {
    case "M0" => m0_eip
    case "J1" => j1_eip
    case "J2" => j2_eip
    case "J3" => j3_eip
    case "K1" => k1_eip
    case "K2" => k2_eip
    case "J4" => j4_eip
    case "J5" => j5_eip
    case "J6" => j6_eip
    case "K2J6" => k2j6_eip
    case "J7" => j7_eip
    case "J8" => j8_eip
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

  def runDirectToDisc(targetDirectory: Path): IO[ExitCode] = {
    for {
      _ <- directToDiskTransaction(targetDirectory)
    } yield ExitCode.Success
  }

  def endProgram(): IO[ExitCode] = {
    // this should properly terminate program from within sbt (all background threads)
    // unfortunately it kills the sbt process in which it runs. back to drawing board
    IO(System.exit(0)).as(ExitCode.Success)
  }
}

object GenerateAllJ extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {

    val approaches = Seq("graphviz","oo","visitor","visitorSideEffect","extensibleVisitor","interpreter","coco","trivially","dispatch","algebra")
    val evolutions = Seq("M0","J1","J2","J3","K1","K2","J4","J5","J6","K2J6","J7","J8")

    approaches.foreach(approach => {
      println("Generating " + approach + "...")
      evolutions.foreach(selection => {
        println("   " + selection)

        val targetDirectory = Paths.get("target", "ep-all", approach, selection)
        val program :IO[Unit] = {
          for {
            _ <- IO { print("Initializing Generator...") }
            main <- IO {  new MainJ(approach, selection) }

            _ <- IO { println("[OK]") }
            result <- main.runDirectToDisc(targetDirectory)
          } yield result
        }

        // execute above as a stand-alone program
        program.unsafeRunSync()

        // TBD:  Would be nice to launch 'sbt' in each of these generated directories
      })
    })

    for {
      _ <- IO { print("DONE") }
    } yield ExitCode.Success

  }
}

object DirectToDiskMainJ extends IOApp {
  val targetDirectory = Paths.get("target", "ep2")

  def run(args: List[String]): IO[ExitCode] = {
    val approach = if (args.isEmpty) "algebra" else args.head
    if (approach == "exit") { sys.exit(0) }
    val selection = if (args.isEmpty || args.tail.isEmpty) "J8" else args.tail.head

    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new MainJ(approach, selection) }
      _ <- IO { println("[OK]") }
      result <- main.runDirectToDisc(targetDirectory)
      //_ <- main.endProgram()
    } yield result

  }
}
