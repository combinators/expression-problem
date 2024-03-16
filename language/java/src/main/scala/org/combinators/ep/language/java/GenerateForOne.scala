package org.combinators.ep.language.java   /*DD:LD:AD*/

import cats.effect.{ExitCode, IO, IOApp}
import org.combinators.ep.generator.FileWithPathPersistable
import FileWithPathPersistable._

import java.nio.file.Paths

/**
 * Execute this object and it will generate subdirectories in "target/ep-one" for each approach you have selected.
 *
 * Note that only include "oo", "visitor", "visitorSideEffect" and "dispatch" for completeness
 *
 * upon completion, in the subdirectory "target/ep-one/approach" there is a "runAll.bat" file that you can
 * execute and capture all output in a file, like "runAll.bat > RESULT". This BAT file will invoke "sbt test"
 * on each of the subdirectories. if the string "[error" ever appears in the RESULT file, then there was some
 * compilation or testing error. If no "[error" appears, then the approach succeeds for all known dependencies. NICE!
 *
 * Known approaches (as of 2024-03-15):
 *
 * "extensibleVisitor", "interpreter", "coco", "trivially", "algebra"
 */
object GenerateForOne extends IOApp {

  val approaches: Seq[String] = Seq("interpreter")

  var stages:Seq[String] = Seq.empty

  def run(args: List[String]): IO[ExitCode] = {
    for {
      _ <- runAll()
      _ <- runD1D2()
      _ <- runThirdAlternate()
      _ <- runProducer()
      _ <- runJournalPaper()
      _ <- generateCommand()
    } yield ExitCode.Success
  }

  def generateCommand() : IO[ExitCode] = {
    approaches.foreach(approach => {

      val targetDirectory = Paths.get("target", "ep-one", approach, "runAll.bat")
      val fileWriter = new java.io.PrintWriter(targetDirectory.toFile)
      stages.foreach(stage => {
        fileWriter.println(f"cd $stage")
        fileWriter.println("call sbt test")
        fileWriter.println("cd ..")
      })
      fileWriter.close()
    })

    for {
      _ <- IO { print("DONE") }
    } yield ExitCode.Success
  }

  def runAll(): IO[ExitCode] = {

    val evolutions = Seq("M0","M1","M2","M3","M4","M5","M6","M7","M7I2","M8","M9","I1","A1","A1M3","A1M3I2","A3","I2",
      "O1","O2","OA","O1OA","OD1","OD2","OD3","OO1","OO2","OO3")
    stages = stages ++ evolutions

    approaches.foreach(approach => {
      println("Generating " + approach + "...")
      evolutions.foreach(selection => {
        println("   " + selection)

        val targetDirectory = Paths.get("target", "ep-one", approach, selection)
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
      })
    })

    for {
      _ <- IO { print("DONE") }
    } yield ExitCode.Success

  }

  def runD1D2(): IO[ExitCode] = {

    val evolutions = Seq("M0","M1","D1","D2","D1D2","D3")
    stages = stages ++ evolutions

    approaches.foreach(approach => {
      println("Generating " + approach + "...")
      evolutions.foreach(selection => {
        println("   " + selection)

        val targetDirectory = Paths.get("target", "ep-one", approach, selection)
        val program :IO[Unit] = {
          for {
            _ <- IO { print("Initializing Generator...") }
            main <- IO {  new MainD1D2(approach, selection) }

            _ <- IO { println("[OK]") }
            result <- main.runDirectToDisc(targetDirectory)
          } yield result
        }

        // execute above as a stand-alone program
        program.unsafeRunSync()
      })
    })

    for {
      _ <- IO { print("DONE") }
    } yield ExitCode.Success
  }

  def runThirdAlternate(): IO[ExitCode] = {
    val evolutions = Seq("M0","X1","X2","X3","X2X3","X4")
    stages = stages ++ evolutions

    approaches.foreach(approach => {
      println("Generating " + approach + "...")
      evolutions.foreach(selection => {
        println("   " + selection)

        val targetDirectory = Paths.get("target", "ep-one", approach, selection)
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
      })
    })

    for {
      _ <- IO { print("DONE") }
    } yield ExitCode.Success
  }

  def runProducer(): IO[ExitCode] = {
    val evolutions = Seq("M0","M1","M2","M3","W1","M3W1","Q1","C2","V1")
    stages = stages ++ evolutions

    approaches.foreach(approach => {
      println("Generating " + approach + "...")
      evolutions.foreach(selection => {
        println("   " + selection)

        val targetDirectory = Paths.get("target", "ep-one", approach, selection)
        val program :IO[Unit] = {
          for {
            _ <- IO { print("Initializing Generator...") }
            main <- IO {  new MainProducer(approach, selection) }

            _ <- IO { println("[OK]") }
            result <- main.runDirectToDisc(targetDirectory)
          } yield result
        }

        // execute above as a stand-alone program
        program.unsafeRunSync()
      })
    })

    for {
      _ <- IO { print("DONE") }
    } yield ExitCode.Success
  }

  def runJournalPaper(): IO[ExitCode] = {
    val evolutions = Seq("M0","M1","M2","I1","I2","N1","M2_ABS","M3","M3I1","I2M3I1N1")
    stages = stages ++ evolutions

    approaches.foreach(approach => {
      println("Generating " + approach + "...")
      evolutions.foreach(selection => {
        println("   " + selection)

        val targetDirectory = Paths.get("target", "ep-one", approach, selection)
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
      })
    })

    for {
      _ <- IO { print("DONE") }
    } yield ExitCode.Success
  }
}