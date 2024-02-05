package org.combinators.ep.language.java    /*DD:LD:AD*/

import cats.effect.{ExitCode, IO, IOApp}
import org.combinators.ep.domain.{WithDomain, math}
import org.combinators.ep.generator.FileWithPath

import java.nio.file.{Path, Paths}
import org.combinators.ep.generator.FileWithPathPersistable._
import org.combinators.ep.language.java.interpreter.{InterpreterGenerator, InterpreterTestGenerator}


class Main {

  val all = new WithDomain(math.MathDomain) with InterpreterGenerator with InterpreterTestGenerator with e0 with e1 with e2 with e3 with e4 with e5
// NOT YET WORKING
//  def transaction[T](initialTransaction: T, addToTransaction: (T, String, () => Seq[FileWithPath]) => T): T = {
//    all.foldLeft(initialTransaction) { case (transaction, (evolution, tests)) =>
//      val impl =
//        () => generator.paradigm.runGenerator {
//          for {
//            _ <- approach.implement(evolution.getModel, eip)
//            _ <- approach.implement(
//              tests,
//              TestImplementationProvider.defaultAssertionBasedTests(approach.paradigm)(generator.assertionsInMethod, generator.equalityInMethod, generator.booleansInMethod, generator.stringsInMethod)
//            )
//          } yield ()
//        }
//      addToTransaction(transaction, evolution.getModel.name, impl)
//    }
//  }
//  val persistable = FileWithPathPersistable[FileWithPath]
//
//  def directToDiskTransaction(targetDirectory: Path): IO[Unit] = {
//    transaction[IO[Unit]](IO.unit, (transaction, evolutionName, files) => IO {
//      print("Computing Files...")
//      val computed = files()
//      println("[OK]")
//      if (targetDirectory.toFile.exists()) {
//        print(s"Cleaning Target Directory (${targetDirectory})...")
//        FileUtils.deleteDirectory(targetDirectory.toFile)
//        println("[OK]")
//      }
//      print("Persisting Files...")
//      files().foreach(file => persistable.persistOverwriting(targetDirectory, file))
//      println("[OK]")
//    })
//  }

//  def runDirectToDisc(targetDirectory: Path): IO[ExitCode] = {
//    for {
//      _ <- directToDiskTransaction(targetDirectory)
//    } yield ExitCode.Success
//  }
}

//object DirectToDiskMain extends IOApp {
//  val targetDirectory = Paths.get("target", "ep2")
//
//  def run(args: List[String]): IO[ExitCode] = {
//    for {
//      _ <- IO { print("Initializing Generator...") }
//      main <- IO { new Main() }
//      _ <- IO { println("[OK]") }
//      result <- main.runDirectToDisc(targetDirectory)
//    } yield result
//  }
//}