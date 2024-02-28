package org.combinators.ep.language.java     /*DD:LD:AD*/

import cats.effect.{ExitCode, IO, IOApp}
import org.combinators.ep.approach.oo.{CoCoClean, ExtensibleVisitor, Interpreter, ObjectAlgebras, RuntimeDispatch, Traditional, TriviallyClean, Visitor, Visualize}
import org.combinators.ep.domain.{Evolution, GenericModel}
import org.combinators.ep.domain.abstractions.TestCase
import org.combinators.ep.domain.math._
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider, FileWithPath, FileWithPathPersistable, NameProvider, TestImplementationProvider}
import org.combinators.jgitserv.{BranchTransaction, GitService}
import FileWithPathPersistable._
import org.apache.commons.io.FileUtils
import org.combinators.ep.approach.oo.Visualize.WithParadigm
import org.combinators.ep.domain.math.{M0, eips}
import org.combinators.ep.domain.math.systemI.{I1, I2}
import org.combinators.ep.domain.math.systemO.{O1, O2}

import java.nio.file.{Path, Paths}
import scala.sys.process.Process

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 */
class Main(choice:String, select:String) {
  val generator: CodeGenerator = CodeGenerator(CodeGenerator.defaultConfig.copy(boxLevel = PartiallyBoxed))

  val visualizeApproach: WithParadigm[generator.paradigm.type] = Visualize[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm)

  val ooApproach: Traditional.WithParadigm[generator.paradigm.type] = Traditional[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm)
  // can't have all of these together
  val visitorApproach: Visitor.WithParadigm[generator.paradigm.type] = Visitor[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  val visitorSideEffectApproach: Visitor.WithParadigm[generator.paradigm.type] = Visitor.withSideEffects[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.imperativeInMethod, generator.ooParadigm)
  val extensibleVisitorApproach: ExtensibleVisitor.WithParadigm[generator.paradigm.type] = ExtensibleVisitor[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  val interpreterApproach: Interpreter.WithParadigm[generator.paradigm.type] = Interpreter[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)

  val cocoCleanApproach: CoCoClean.WithParadigm[generator.paradigm.type] = CoCoClean[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  val triviallyCleanApproach: TriviallyClean.WithParadigm[generator.paradigm.type] = TriviallyClean[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm)

  val dispatchApproach: RuntimeDispatch.WithParadigm[generator.paradigm.type] = RuntimeDispatch[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.imperativeInMethod, generator.stringsInMethod, generator.exceptionsInMethod, generator.ooParadigm)
  val algebraApproach: ObjectAlgebras.WithParadigm[generator.paradigm.type] = ObjectAlgebras[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)

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

  val evolutions: Seq[Evolution] = select match {
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

    case "O1" => Seq(M0, M1, M2, O1)
    case "O2" => Seq(M0, M1, M2, O1, O2)

    case _ => ???
  }

  //  val eip = eips.I2(approach.paradigm)(generator.doublesInMethod, generator.realDoublesInMethod,
//    generator.stringsInMethod, generator.imperativeInMethod)
//  // how do I just use M2 instead of this? HACK
  val m0_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.M0(approach.paradigm)(generator.doublesInMethod,generator.stringsInMethod)
  val m1_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.M1(approach.paradigm)(m0_eip)(generator.doublesInMethod)
  val m2_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.M2(approach.paradigm)(m1_eip)(generator.doublesInMethod, generator.stringsInMethod)

  //val m2_abs_eip = eips.M2_ABS(approach.paradigm)(m2_eip)(generator.doublesInMethod, generator.imperativeInMethod, generator.stringsInMethod)

  val m3_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.M3(approach.paradigm)(m2_eip)(generator.doublesInMethod, generator.stringsInMethod)
  val o1_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.systemO.O1(approach.paradigm)(m2_eip)(generator.doublesInMethod, generator.stringsInMethod)
  val o2_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.systemO.O2(approach.paradigm)(o1_eip)(generator.doublesInMethod, generator.stringsInMethod)

  val m4_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.M4.imperative[approach.paradigm.type,ApproachImplementationProvider.WithParadigm](approach.paradigm)(m3_eip)(
      generator.imperativeInMethod,
      generator.doublesInMethod,
      generator.booleansInMethod,
      generator.stringsInMethod,
      generator.listsInMethod,
      generator.equalityInMethod)
  val m5_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.M5(approach.paradigm)(m4_eip)(generator.intsInMethod,generator.treesInMethod)
  val m6_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.M6(approach.paradigm)(m5_eip)(generator.equalityInMethod, generator.booleansInMethod)
  val m7_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.M7(approach.paradigm)(m6_eip)(generator.doublesInMethod, generator.realDoublesInMethod, generator.stringsInMethod, generator.imperativeInMethod)
  val i1_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.systemI.I1(approach.paradigm)(m2_eip)(generator.doublesInMethod, generator.realDoublesInMethod, generator.stringsInMethod, generator.imperativeInMethod)
  val i2_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.systemI.I2(approach.paradigm)(i1_eip)(generator.doublesInMethod, generator.realDoublesInMethod, generator.stringsInMethod, generator.imperativeInMethod)
  val m7i2_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.M7I2.imperative[approach.paradigm.type,ApproachImplementationProvider.WithParadigm](approach.paradigm)(m7_eip,i2_eip)(
    generator.imperativeInMethod,
    generator.doublesInMethod,
    generator.booleansInMethod,
    generator.equalityInMethod)
  val m8_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.M8.imperative[approach.paradigm.type,ApproachImplementationProvider.WithParadigm](approach.paradigm)(m7i2_eip)(
    generator.imperativeInMethod,
    generator.doublesInMethod,
    generator.booleansInMethod,
    generator.stringsInMethod,
    generator.equalityInMethod)
  val m9_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.M9(approach.paradigm)(m8_eip)(generator.doublesInMethod, generator.realDoublesInMethod, generator.imperativeInMethod)

  val a1_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.A1(approach.paradigm)(i1_eip)(generator.doublesInMethod, generator.stringsInMethod)
  val a1m3_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.A1M3(approach.paradigm)(m3_eip, a1_eip)(generator.stringsInMethod)
  val a1m3i2_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.A1M3I2(approach.paradigm)(a1m3_eip, i2_eip)(generator.stringsInMethod)
  val a3_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.A3(approach.paradigm)(a1m3i2_eip)(generator.doublesInMethod, generator.stringsInMethod)

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
    case "O1" => o1_eip
    case "O2" => o2_eip

    case _ => ???
  }

  val tests: Seq[Map[GenericModel, Seq[TestCase]]] = evolutions.scanLeft(Map.empty[GenericModel, Seq[TestCase]]) { case (m, evolution) =>
    m + (evolution.getModel -> evolution.tests)
  }.tail

  // for CoCo, we only need the latest since all earlier ones are identical
  val all: Seq[(Evolution, Map[GenericModel, Seq[TestCase]])] = evolutions.zip(tests)

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

  val persistable: Aux[FileWithPath] = FileWithPathPersistable[FileWithPath]

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
    val approach = if (args.isEmpty) "interpreter" else args.head
    val selection = if (args.isEmpty || args.tail.isEmpty) "M7I2" else args.tail.head
    new Main(approach, selection).runGit(args)
  }
}

object GenerateAll extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {

    val approaches = Seq("graphviz","oo","visitor","visitorSideEffect","extensibleVisitor","interpreter","coco","trivially","dispatch","algebra")
    val evolutions = Seq("M0","M1","M2","M3","M4","M5","M6","M7","M7I2","M8","M9","I1","A1","A1M3","A1M3I2","A3","I2")

    approaches.foreach(approach => {
      println("Generating " + approach + "...")
      evolutions.foreach(selection => {
        println("   " + selection)

        val targetDirectory = Paths.get("target", "ep-all", approach, selection)
        val program :IO[Unit] = {
          for {
            _ <- IO { print("Initializing Generator...") }
            main <- IO {  new Main(approach, selection) }

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

object DirectToDiskMain extends IOApp {
  val targetDirectory: Path = Paths.get("target", "ep3")

  def run(args: List[String]): IO[ExitCode] = {

    // extensibleVisitor, trivially, interpreter all have issues with O1.
    val approach = if (args.isEmpty) "extensibleVisitor" else args.head
    if (approach == "exit") { sys.exit(0) }
    val selection = if (args.isEmpty || args.tail.isEmpty) "O2" else args.tail.head
    println("Generating " + approach + " for " + selection)
    val main = new Main(approach, selection)

    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { main }

      _ <- IO { println("[OK]") }
      result <- main.runDirectToDisc(targetDirectory)
    } yield result
  }
}
