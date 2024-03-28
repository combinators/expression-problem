package org.combinators.ep.language.java     /*DC:LD:AD*/

import cats.effect.{ExitCode, IO, IOApp}
import org.apache.commons.io.FileUtils
import org.combinators.ep.approach.oo.{CoCoClean, ExtensibleVisitor, Interpreter, ObjectAlgebras, RuntimeDispatch, Traditional, TriviallyClean, Visitor, Visualize}
import org.combinators.ep.domain.{Evolution, GenericModel}
import org.combinators.ep.domain.abstractions.TestCase
import org.combinators.ep.domain.math.eips.systemX
import org.combinators.ep.domain.math.systemX.{X1, X2, X2X3, X3, X4}
import org.combinators.ep.domain.math.{M0, eips}
import org.combinators.ep.generator.ApproachImplementationProvider.WithParadigm
import org.combinators.ep.generator.{EvolutionImplementationProvider, FileWithPath, FileWithPathPersistable, TestImplementationProvider}
import org.combinators.jgitserv.{BranchTransaction, GitService}
import org.combinators.ep.generator.FileWithPathPersistable._

import java.nio.file.{Path, Paths}

class MainThirdAlternate(choice:String, select:String) {
  val generator: CodeGenerator = CodeGenerator(CodeGenerator.defaultConfig.copy(boxLevel = PartiallyBoxed))
  val visualizeApproach: Visualize.WithParadigm[generator.paradigm.type] = Visualize[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm)

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
    case "X1" => Seq(M0, X1)
    case "X2" => Seq(M0, X1, X2)
    case "X3" => Seq(M0, X1, X3)
    case "X2X3" => Seq(M0, X1, X2, X3, X2X3)
    case "X4" => Seq(M0, X1, X2, X3, X2X3, X4)
    case _ => ???
  }

  val m0_eip: EvolutionImplementationProvider[WithParadigm[approach.paradigm.type]] =
    eips.M0(approach.paradigm)(generator.doublesInMethod, generator.stringsInMethod)

  val x1_eip: EvolutionImplementationProvider[WithParadigm[approach.paradigm.type]] =
    systemX.X1(approach.paradigm)(m0_eip)(generator.doublesInMethod, generator.realDoublesInMethod, generator.stringsInMethod, generator.imperativeInMethod)
  val x2_eip: EvolutionImplementationProvider[WithParadigm[approach.paradigm.type]] =
    systemX.X2(approach.paradigm)(x1_eip)(generator.doublesInMethod, generator.stringsInMethod)
  val x3_eip: EvolutionImplementationProvider[WithParadigm[approach.paradigm.type]] =
    systemX.X3(approach.paradigm)(x1_eip)(generator.doublesInMethod, generator.stringsInMethod)

  val x2x3_eip: EvolutionImplementationProvider[WithParadigm[approach.paradigm.type]] =
    systemX.X2X3(approach.paradigm)(x2_eip, x3_eip)

  val x4_eip: EvolutionImplementationProvider[WithParadigm[approach.paradigm.type]] =
    systemX.X4(approach.paradigm)(x2x3_eip)(generator.doublesInMethod, generator.stringsInMethod)

  val eip: EvolutionImplementationProvider[WithParadigm[approach.paradigm.type]] = x4_eip

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

object GenerateAllThirdAlternate extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {

    val approaches = Seq("graphviz","oo","visitor","visitorSideEffect","extensibleVisitor","interpreter","coco","trivially","dispatch","algebra")
    val evolutions = Seq("M0","X1","X2","X3","X2X3","X4")

    approaches.foreach(approach => {
      println("Generating " + approach + "...")
      evolutions.foreach(selection => {
        println("   " + selection)

        val targetDirectory = Paths.get("target", "ep-all", approach, selection)
        val program :IO[Unit] = {
          for {
            _ <- IO { print("Initializing Generator...") }
            main <- IO {  new MainThirdAlternate(approach, selection) }

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

object DirectToDiskMainThirdAlternate extends IOApp {
  val targetDirectory: Path = Paths.get("target", "ep2")

  // fails on X1, generating multiple statements that are unnecessary and even do not compile for X1
  def run(args: List[String]): IO[ExitCode] = {
    val approach = if (args.isEmpty) "visitorSideEffect" else args.head
    if (approach == "exit") { sys.exit(0) }
    val selection = if (args.isEmpty || args.tail.isEmpty) "X1" else args.tail.head

    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new MainThirdAlternate(approach, selection) }
      _ <- IO { println("[OK]") }
      result <- main.runDirectToDisc(targetDirectory)
    } yield result
  }
}