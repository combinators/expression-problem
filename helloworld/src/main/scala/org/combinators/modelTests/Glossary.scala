package org.combinators.modelTests

/**
 * sbt "dp/runMain org.combinators.dp.DPJavaDirectToDiskMain"
 *
 * Creates output files in target/dp
 */

import cats.effect.{ExitCode, IO, IOApp}
import org.apache.commons.io.FileUtils
import org.combinators.dp.{BottomUp, LCSMainJava, TopDown}
import org.combinators.ep.generator.{FileWithPath, FileWithPathPersistable}
import org.combinators.model.models.twoSequences.{LongestCommonSubsequenceModel, UncrossedLinesModel}
import org.combinators.modelTests.uncrossedLines.UncrossedLinesMainJava

import org.combinators.ep.generator.FileWithPathPersistable._

import java.nio.file.{Path, Paths}
import scala.collection.Seq

class Glossary {

  val persistable = FileWithPathPersistable[FileWithPath]

  def directToDiskTransaction(targetDirectory: Path, files: Seq[FileWithPath]): IO[Unit] = {
    IO {
      print("Computing Files...")
      println("[OK]")
      if (targetDirectory.toFile.exists()) {
        print(s"Cleaning Target Directory ($targetDirectory)...")
        FileUtils.deleteDirectory(targetDirectory.toFile)
        println("[OK]")
      }
      print("Persisting Files...")
      files.foreach(file => persistable.persistOverwriting(targetDirectory, file))
      println("[OK]")
    }
  }

  def runDirectToDisc(targetDirectory: Path,files: Seq[FileWithPath]): IO[ExitCode] = {
    for {
      _ <- directToDiskTransaction(targetDirectory, files)
    } yield ExitCode.Success
  }
}

object GlossaryToDiskMain extends IOApp {
  val targetDirectory:Path = Paths.get("target", "dp")

  // choose one of these to pass in
  val topDown         = new TopDown()
  val topDownWithMemo = new TopDown(memo = true)
  val bottomUp        = new BottomUp()

  // below are the individual DP problems generated and added to `all_files`.
  val ul = new UncrossedLinesMainJava().filesToGenerate(new UncrossedLinesModel().instantiate(), topDown)
  val lcs = new LCSMainJava().filesToGenerate(new LongestCommonSubsequenceModel().instantiate(), topDown)

  // add them all together
  val all_files = ul ++ lcs

  def run(args: List[String]): IO[ExitCode] = {
    for {
        _ <- IO { println("Initializing Generator...") }
        main <- IO { new Glossary() }
        _ <- IO { println("[OK]") }

        result <- main.runDirectToDisc(targetDirectory, all_files)
      } yield result
    }
}
