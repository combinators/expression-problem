package org.combinators.ep.language.java     /*DD:LD:AD*/

import cats.effect.{ExitCode, IO, IOApp}
import org.combinators.ep.approach.oo.{CoCoClean, ExtensibleVisitor, Interpreter, ObjectAlgebras, RuntimeDispatch, Traditional, TriviallyClean, Visitor, Visualize}
import org.combinators.ep.domain.{Evolution, GenericModel}
import org.combinators.ep.domain.abstractions.TestCase
import org.combinators.ep.domain.math.{eips, _}
import org.combinators.ep.generator.{ApproachImplementationProvider, EvolutionImplementationProvider, FileWithPath, FileWithPathPersistable, TestImplementationProvider}
import org.combinators.jgitserv.{BranchTransaction, GitService}
import FileWithPathPersistable._
import org.apache.commons.io.FileUtils
import org.combinators.ep.approach.oo.Visualize.WithParadigm
import org.combinators.ep.domain.math.systemI.{I1, I2}

import java.nio.file.{Path, Paths}

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 */
class MainJournalPaper(choice:String, select:String) {
  val generator: CodeGenerator = CodeGenerator(CodeGenerator.defaultConfig.copy(boxLevel = PartiallyBoxed))
  val visualizeApproach: WithParadigm[generator.paradigm.type] = Visualize[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm)

  val ooApproach: Traditional.WithParadigm[generator.paradigm.type] = Traditional[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm)
  val visitorApproach: Visitor.WithParadigm[generator.paradigm.type] = Visitor[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  val visitorSideEffectApproach: Visitor.WithParadigm[generator.paradigm.type] = Visitor.withSideEffects[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.imperativeInMethod, generator.ooParadigm)
  val extensibleVisitorApproach: ExtensibleVisitor.WithParadigm[generator.paradigm.type] = ExtensibleVisitor[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  val interpreterApproach: Interpreter.WithParadigm[generator.paradigm.type] = Interpreter[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
  val triviallyCleanApproach: TriviallyClean.WithParadigm[generator.paradigm.type] = TriviallyClean[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm)

  val dispatchApproach: RuntimeDispatch.WithParadigm[generator.paradigm.type] = RuntimeDispatch[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.imperativeInMethod, generator.stringsInMethod, generator.exceptionsInMethod, generator.ooParadigm)

  val cocoCleanApproach: CoCoClean.WithParadigm[generator.paradigm.type] = CoCoClean[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)
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

  val evolutions = select match {
    case "M0" => Seq(M0)
    case "M1" => Seq(M0, M1)
    case "M2" => Seq(M0, M1, M2)
    case "I1" => Seq(M0, M1, M2, I1)
    case "I2" => Seq(M0, M1, M2, I1, I2)
    case "N1" => Seq(M0, M1, M3, N1)
    case "M2_ABS" => Seq(M0, M1, M2, M2_ABS)
    case "M3" => Seq(M0, M1, M2, M3)
    case "M3I1" => Seq(M0, M1, M2, M3, I1, M3I1)
    case "I2M3I1N1" => Seq(M0, M1, M2, M3, I1, M3I1, I2, N1, I2M3I1N1)

    case _ => ???
  }

  val m0_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.M0(approach.paradigm)(generator.doublesInMethod, generator.stringsInMethod)
  val m1_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.M1(approach.paradigm)(m0_eip)(generator.doublesInMethod)
  val m2_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.M2(approach.paradigm)(m1_eip)(generator.doublesInMethod, generator.stringsInMethod)

  val m2_abs_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.M2_ABS(approach.paradigm)(m2_eip)(generator.doublesInMethod, generator.imperativeInMethod, generator.stringsInMethod)

  val m3_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.M3(approach.paradigm)(m2_eip)(generator.doublesInMethod, generator.stringsInMethod)

  val i1_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.systemI.I1(approach.paradigm)(m2_eip)(generator.doublesInMethod, generator.realDoublesInMethod, generator.stringsInMethod, generator.imperativeInMethod)
  val i2_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.systemI.I2(approach.paradigm)(i1_eip)(generator.doublesInMethod, generator.realDoublesInMethod, generator.stringsInMethod, generator.imperativeInMethod)

  val m3i1_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.M3I1.imperative[approach.paradigm.type,ApproachImplementationProvider.WithParadigm](approach.paradigm)(m3_eip,i1_eip)(
    generator.imperativeInMethod,
    generator.booleansInMethod,
    generator.equalityInMethod,
    generator.stringsInMethod
  )

  val n1_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.N1(approach.paradigm)(m3_eip)(generator.doublesInMethod, generator.realDoublesInMethod, generator.stringsInMethod, generator.imperativeInMethod)

  val i2m3i1n1_eip: EvolutionImplementationProvider[ApproachImplementationProvider.WithParadigm[approach.paradigm.type]] =
    eips.I2M3I1N1.imperative[approach.paradigm.type,ApproachImplementationProvider.WithParadigm](approach.paradigm)(i2_eip,m3i1_eip,n1_eip)(
        generator.booleansInMethod,
        generator.equalityInMethod,
        generator.stringsInMethod
  )

  //val eip = a1m3_eip
  val eip = select match {
    case "M0" => m0_eip
    case "M1" => m1_eip
    case "M2" => m2_eip
    case "I1" => i1_eip
    case "I2" => i2_eip
    case "N1" => n1_eip
    case "M2_ABS" => m2_abs_eip
    case "M3" => m3_eip
    case "M3I1" => m3i1_eip
    case "I2M3I1N1" => i2m3i1n1_eip
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
      computed.foreach(file => persistable.persistOverwriting(targetDirectory, file))
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

object GitMainJournalPaperProducer extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    val approach = if (args.isEmpty) "algebra" else args.head
    val selection = if (args.isEmpty || args.tail.isEmpty) "I2M3I1N1" else args.tail.head

    new MainJournalPaper(approach, selection).runGit(args)
  }
}


object GenerateAllJournalPaper extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {

    val approaches = Seq("graphviz","oo","visitor","visitorSideEffect","extensibleVisitor","interpreter","coco","trivially","dispatch","algebra")
    val evolutions = Seq("M0","M1","M2","I1","I2","N1","M2_ABS","M3","M3I1","I2M3I1N1")

    approaches.foreach(approach => {
      println("Generating " + approach + "...")
      evolutions.foreach(selection => {
        println("   " + selection)

        val targetDirectory = Paths.get("target", "ep-all", approach, selection)
        val program :IO[Unit] = {
          for {
            _ <- IO { print("Initializing Generator...") }
            main <- IO {  new MainJournalPaper(approach, selection) }

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

object DirectToDiskMainJournalPaper extends IOApp {
  val targetDirectory: Path = Paths.get("target", "ep2")

  // I2M3I1N1 caused problems for algebra. Problem is in findDomainToUse when a merge point has a past merge point...

  def run(args: List[String]): IO[ExitCode] = {
    val approach = if (args.isEmpty) "algebra" else args.head
    if (approach == "exit") { sys.exit(0) }
    val selection = if (args.isEmpty || args.tail.isEmpty) "I2M3I1N1" else args.tail.head

    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new MainJournalPaper(approach, selection) }
      _ <- IO { println("[OK]") }
      result <- main.runDirectToDisc(targetDirectory)
    } yield result
  }
}