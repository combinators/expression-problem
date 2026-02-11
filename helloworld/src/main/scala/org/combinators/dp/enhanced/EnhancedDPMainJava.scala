package org.combinators.dp.enhanced

/**
 * sbt "dp/runMain org.combinators.dp.DPJavaDirectToDiskMain"
 *
 * Creates output files in target/dp
 */

import cats.effect.{ExitCode, IO, IOApp}
import com.github.javaparser.ast.PackageDeclaration
import org.apache.commons.io.FileUtils
import org.combinators.dp.{BottomUp, GenerationOption, TestExample, TopDown}
import org.combinators.ep.generator.FileWithPathPersistable._
import org.combinators.ep.generator.{FileWithPath, FileWithPathPersistable}
import org.combinators.ep.language.java.paradigm.ObjectOriented
import org.combinators.ep.language.java.{CodeGenerator, JavaNameProvider, PartiallyBoxed, Syntax}
import org.combinators.model._

import java.nio.file.{Path, Paths}

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 */
abstract class EnhancedDPMainJava {
  val generator = CodeGenerator(CodeGenerator.defaultConfig.copy(boxLevel = PartiallyBoxed, targetPackage = new PackageDeclaration(ObjectOriented.fromComponents("dp"))))

  val dpApproach = EnhancedDPObjectOrientedProvider[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.imperativeInMethod, generator.doublesInMethod, generator.realDoublesInMethod, generator.consoleInMethod, generator.arraysInMethod, generator.assertionsInMethod, generator.stringsInMethod, generator.equalityInMethod, generator.ooParadigm, generator.parametricPolymorphism, generator.booleansInMethod)(generator.generics)

  val persistable = FileWithPathPersistable[FileWithPath]

  // subclasses will provide tests
  def tests:Seq[TestExample]

  def filesToGenerate(model: EnhancedModel, option: GenerationOption): Seq[FileWithPath] = {
    println(s"Generating ${model.problem}...")
    generator.paradigm.runGenerator {
      for {
        _ <- generator.doublesInMethod.enable()
        _ <- generator.realDoublesInMethod.enable()
        _ <- generator.intsInMethod.enable()
        _ <- generator.stringsInMethod.enable()
        _ <- generator.listsInMethod.enable() // should be array, but this still needs to be added as an FFI
        _ <- generator.consoleInMethod.enable()
        _ <- generator.arraysInMethod.enable()
        _ <- generator.equalityInMethod.enable()
        _ <- generator.assertionsInMethod.enable()
        _ <- generator.booleansInMethod.enable()

        // HERE you can finally specify the method to use for testing and the test cases
        _ <- dpApproach.implement(model, tests, option)
      } yield ()
    }
  }

  def directToDiskTransaction(targetDirectory: Path, model:EnhancedModel, option:GenerationOption): IO[Unit] = {

     IO {
      print("Computing Files...")
      val computed = filesToGenerate(model, option)
      println("[OK]")
      if (targetDirectory.toFile.exists()) {
        print(s"Cleaning Target Directory ($targetDirectory)...")
        FileUtils.deleteDirectory(targetDirectory.toFile)
        println("[OK]")
      }
      print("Persisting Files...")
      computed.foreach(file => persistable.persistOverwriting(targetDirectory, file))
      println("[OK]")
    }
  }

  def runDirectToDisc(targetDirectory: Path, model:EnhancedModel, option:GenerationOption): IO[ExitCode] = {
    for {
      _ <- directToDiskTransaction(targetDirectory, model, option)
    } yield ExitCode.Success
  }
}


