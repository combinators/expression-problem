package org.combinators.ep.language.java

/* Generates Hello World program. */

/** To truly make this work as expected, need to have a CONSOLE concept (much like getSelf()) which would be contextualized on each platform
 * 
 * On Java it would retrieve "System.out" for output or "System.in" for input. Punting on this right now...
 * 
 * */

import cats.effect.{ExitCode, IO, IOApp}
import org.combinators.ep.generator.{FileWithPath, FileWithPathPersistable}
import FileWithPathPersistable._
import org.apache.commons.io.FileUtils
import org.combinators.ep.generator.helloworld._
import java.nio.file.{Path, Paths}

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 */
class HelloWorldMain {
  val generator = CodeGenerator(CodeGenerator.defaultConfig.copy(boxLevel = PartiallyBoxed))
  
  val helloWorldApproach = HelloWorldObjectOrientedProvider[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.ooParadigm)

  val persistable = FileWithPathPersistable[FileWithPath]

  def directToDiskTransaction(targetDirectory: Path): IO[Unit] = {
    val files =
      () => generator.paradigm.runGenerator {
        for {
          _ <- generator.doublesInMethod.enable()
          _ <- generator.stringsInMethod.enable()
          _ <- generator.listsInMethod.enable()     // should be array, but this still needs to be added as an FFI
          _ <- helloWorldApproach.implement()
        } yield ()
      }

     IO {
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
    }
  }

  def runDirectToDisc(targetDirectory: Path): IO[ExitCode] = {
    for {
      _ <- directToDiskTransaction(targetDirectory)
    } yield ExitCode.Success
  }
}

object HelloWorldDirectToDiskMain extends IOApp {
  val targetDirectory = Paths.get("target", "ep3")

  def run(args: List[String]): IO[ExitCode] = {
    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new HelloWorldMain() }
      _ <- IO { println("[OK]") }
      result <- main.runDirectToDisc(targetDirectory)
    } yield result
  }
}
