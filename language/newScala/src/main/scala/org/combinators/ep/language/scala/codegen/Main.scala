package org.combinators.ep.language.scala.codegen    /*DD:LD:AD*/

import cats.effect.{ExitCode, IO, IOApp}
import org.apache.commons.io.FileUtils
import org.combinators.ep.approach.oo.Traditional.WithParadigm
import org.combinators.ep.approach.oo._
import org.combinators.ep.domain.{Evolution, GenericModel}
import org.combinators.ep.domain.abstractions.TestCase
import org.combinators.ep.domain.math._
import org.combinators.ep.domain.math.systemI.{I1, I2}
import org.combinators.ep.domain.math.systemO.{O1, O1OA, O2, OA, OD1, OD2, OD3, OO1, OO2, OO3}
import org.combinators.ep.generator.FileWithPathPersistable._
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider, FileWithPath, FileWithPathPersistable, TestImplementationProvider}
import org.combinators.jgitserv.{BranchTransaction, GitService}

import java.nio.file.{Path, Paths}

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 */
class Main(choice:String, select:String) {
  val generator: CodeGenerator = CodeGenerator(M0.getModel.base.name.toLowerCase)

  val ooApproach: WithParadigm[generator.paradigm.type] = Traditional[generator.syntax.type, generator.paradigm.type](generator.paradigm)(generator.nameProvider, generator.ooParadigm)
  // can't have all of these together
  val visitorApproach: Visitor.WithParadigm[generator.paradigm.type] = Visitor[generator.syntax.type, generator.paradigm.type](generator.paradigm)(generator.nameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  val visitorSideEffectApproach: Visitor.WithParadigm[generator.paradigm.type] = Visitor.withSideEffects[generator.syntax.type, generator.paradigm.type](generator.paradigm)(generator.nameProvider, generator.imperative, generator.ooParadigm)
  val extensibleVisitorApproach: ExtensibleVisitor.WithParadigm[generator.paradigm.type] = ExtensibleVisitor[generator.syntax.type, generator.paradigm.type](generator.paradigm)(generator.nameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  val interpreterApproach: Interpreter.WithParadigm[generator.paradigm.type] = Interpreter[generator.syntax.type, generator.paradigm.type](generator.paradigm)(generator.nameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)

  val cocoCleanApproach: CoCoClean.WithParadigm[generator.paradigm.type] = CoCoClean[generator.syntax.type, generator.paradigm.type](generator.paradigm)(generator.nameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  val triviallyCleanApproach: TriviallyClean.WithParadigm[generator.paradigm.type] = TriviallyClean[generator.syntax.type, generator.paradigm.type](generator.paradigm)(generator.nameProvider, generator.ooParadigm)

  //val dispatchApproach = RuntimeDispatch[generator.syntax.type, generator.paradigm.type](generator.paradigm)(generator.nameProvider, generator.imperative, generator.strings, generator.exceptions, generator.ooParadigm)
  val algebraApproach: ObjectAlgebras.WithParadigm[generator.paradigm.type] = ObjectAlgebras[generator.syntax.type, generator.paradigm.type](generator.paradigm)(generator.nameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)

  val visualizeApproach: Visualize.WithParadigm[generator.paradigm.type] = Visualize[generator.syntax.type, generator.paradigm.type](generator.paradigm)(generator.nameProvider, generator.ooParadigm)

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
    case "dispatch" => ???  // not yet implemented
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
    case "OA" => Seq(M0, M1, M2, OA)
    case "O1OA" => Seq(M0, M1, M2, O1, OA, O1OA)
    case "O2" => Seq(M0, M1, M2, O1, O2)

    case "OD1" => Seq(M0, M1, M2, OD1)
    case "OD2" => Seq(M0, M1, M2, OD2)
    case "OD3" => Seq(M0, M1, M2, OD1, OD2, OD3)

    case "OO1" => Seq(M0, M1, M2, OO1)
    case "OO2" => Seq(M0, M1, M2, OO2)
    case "OO3" => Seq(M0, M1, M2, OO1, OO2, OO3)

    case _ => ???
  }



  val m0_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.M0(approach.paradigm)(generator.doubles,generator.strings)
  val m1_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.M1(approach.paradigm)(m0_eip)(generator.doubles)
  val m2_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.M2(approach.paradigm)(m1_eip)(generator.doubles, generator.strings)

  val m3_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.M3(approach.paradigm)(m2_eip)(generator.doubles, generator.strings)

  val m4_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.M4.imperative[approach.paradigm.type,ApproachImplementationProvider.WithParadigm](approach.paradigm)(m3_eip)(
      generator.imperative, generator.doubles, generator.booleans, generator.strings, generator.listsInMethod, generator.equality)
  val m5_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.M5(approach.paradigm)(m4_eip)(generator.ints,generator.treesInMethod)
  val m6_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.M6(approach.paradigm)(m5_eip)(generator.equality, generator.booleans)
  val m7_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.M7(approach.paradigm)(m6_eip)(generator.doubles, generator.realDoubles, generator.strings, generator.imperative)
  val i1_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.systemI.I1(approach.paradigm)(m2_eip)(generator.doubles, generator.realDoubles, generator.strings, generator.imperative)
  val i2_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.systemI.I2(approach.paradigm)(i1_eip)(generator.doubles, generator.realDoubles, generator.strings, generator.imperative)
  val m7i2_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.M7I2.imperative[approach.paradigm.type,ApproachImplementationProvider.WithParadigm](approach.paradigm)(m7_eip,i2_eip)(
    generator.imperative, generator.doubles, generator.booleans, generator.equality)
  val m8_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.M8.imperative[approach.paradigm.type,ApproachImplementationProvider.WithParadigm](approach.paradigm)(m7i2_eip)(
    generator.imperative, generator.doubles, generator.booleans, generator.strings, generator.equality)
  val m9_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.M9(approach.paradigm)(m8_eip)(generator.doubles, generator.realDoubles, generator.imperative)

  val a1_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.A1(approach.paradigm)(i1_eip)(generator.doubles, generator.strings)
  val a1m3_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.A1M3(approach.paradigm)(m3_eip, a1_eip)(generator.strings)

  val a1m3i2_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.A1M3I2(approach.paradigm)(a1m3_eip, i2_eip)(generator.strings)
  val a3_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.A3(approach.paradigm)(a1m3i2_eip)(generator.doubles, generator.strings)

  val o1_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.systemO.O1(approach.paradigm)(m2_eip)(generator.doubles, generator.strings)
  val oa_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.systemO.OA(approach.paradigm)(m2_eip)(generator.doubles)
  val o1oa_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.systemO.O1OA(approach.paradigm)(o1_eip, oa_eip)
  val o2_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.systemO.O2(approach.paradigm)(o1_eip)(generator.doubles, generator.strings)

  val od1_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.systemO.OD1(approach.paradigm)(m2_eip)(generator.doubles, generator.strings)
  val od2_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.systemO.OD2(approach.paradigm)(m2_eip)(generator.doubles, generator.strings)
  val od3_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.systemO.OD3(approach.paradigm)(od1_eip, od2_eip)

  val oo1_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.systemO.OO1(approach.paradigm)(m2_eip)(generator.doubles, generator.realDoubles, generator.strings)
  val oo2_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.systemO.OO2(approach.paradigm)(m2_eip)(generator.doubles, generator.realDoubles, generator.strings)
  val oo3_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.systemO.OO3(approach.paradigm)(oo1_eip, oo2_eip)

  val eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] = select match {
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
    case "OA" => oa_eip
    case "O1OA" => o1oa_eip
    case "O2" => o2_eip

    case "OD1" => od1_eip
    case "OD2" => od2_eip
    case "OD3" => od3_eip

    case "OO1" => oo1_eip
    case "OO2" => oo2_eip
    case "OO3" => oo3_eip

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
              TestImplementationProvider.defaultAssertionBasedTests(approach.paradigm)(generator.assertionsInMethod, generator.equality, generator.booleans, generator.strings)
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
    val approach = if (args.isEmpty) "interpreter" else args.head
    val selection = if (args.isEmpty || args.tail.isEmpty) "M7I2" else args.tail.head
    new Main(approach, selection).runGit(args)
  }
}

object DirectToDiskMain extends IOApp {
  val targetDirectory: Path = Paths.get("target", "ep2")

  def run(args: List[String]): IO[ExitCode] = {

    val approach = if (args.isEmpty) "coco" else args.head
    if (approach == "exit") { sys.exit(0) }
    val selection = if (args.isEmpty || args.tail.isEmpty) "O1" else args.tail.head
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

object GenerateAll extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {

    val approaches = Seq("oo","visitor","visitorSideEffect","extensibleVisitor","interpreter","coco","trivially","algebra")
    val evolutions = Seq("M0","M1","M2","M3","M4","M5","M6","M7","M7I2","M8","M9","I1","A1","A1M3","A1M3I2","A3","I2",
      "O1","O2","OA","O1OA","OD1","OD2","OD3","OO1","OO2","OO3")

    approaches.filter(px => px == "coco").foreach(approach => {
      println("Generating " + approach + "...")
      evolutions.foreach(selection => {
        println("   " + selection)

        val targetDirectory = Paths.get("target", "ep-all", approach, selection)
        val program :IO[Unit] = {
          for {
            _ <- IO { print("Initializing Generator...") }
            main <- IO {  new Main(approach, selection) }

            _ <- IO { println("[OK]") }
            _ <- main.runDirectToDisc(targetDirectory)
          } yield ()
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