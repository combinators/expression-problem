package org.combinators.dp.enhanced

import cats.effect.{ExitCode, IO}
import org.apache.commons.io.FileUtils
import org.combinators.ep.language.scala.ast.ffi._
import org.combinators.ep.language.scala.ast.{FinalBaseAST, FinalNameProviderAST}
import org.combinators.ep.language.scala.codegen.CodeGenerator
import org.combinators.ep.language.scala.codegen.FullAST
import org.combinators.cogen._
import org.combinators.cogen.FileWithPathPersistable
import org.combinators.cogen.FileWithPathPersistable.fileWithPathPersistable
import org.combinators.dp.{GenerationOption, TestExample}
import org.combinators.models.EnhancedModel

import java.nio.file.Path

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 */
abstract class EnhancedDPMainScala extends EnhancedMainInterface {
  val _ast: FullAST = new FinalBaseAST
    with FinalNameProviderAST
    with FinalArithmeticAST
    with FinalArraysAST
    with FinalAssertionsAST
    with FinalBooleanAST
    with FinalConsoleAST
    with FinalEqualsAST
    with FinalListsAST
    with FinalOperatorExpressionsAST
    with FinalRealArithmeticOpsAST
    with FinalStringAST {
    val reificationExtensions = List.empty
  }
  val generator: CodeGenerator[_ast.type] = CodeGenerator("dp", _ast, Set.empty)

  val dpApproach = EnhancedDPObjectOrientedProvider[generator.syntax.type, generator.paradigm.type](generator.paradigm)(generator.nameProvider, generator.imperative.imperativeInMethods, generator.doubles.arithmeticInMethods, generator.realDoubles.realArithmeticInMethods, generator.console.consoleInMethods, generator.arrays.arraysInMethods, generator.assertions.assertionsInMethods, generator.strings.stringsInMethods, generator.equality.equalsInMethods, generator.ooParadigm, generator.parametricPolymorphism, generator.booleans.booleansInMethodsInMethods)(generator.generics)

  val persistable = FileWithPathPersistable[FileWithPath]

  // subclasses will provide tests
  def tests:Seq[TestExample]

  def filesToGenerate(model: EnhancedModel, option: GenerationOption): Seq[FileWithPath] = {
    println(s"Generating ${model.problem}...")
    generator.paradigm.runGenerator {
      for {
        _ <- generator.doubles.arithmeticInMethods.enable()
        _ <- generator.realDoubles.realArithmeticInMethods.enable()
        _ <- generator.ints.arithmeticInMethods.enable()
        _ <- generator.strings.stringsInMethods.enable()
        _ <- generator.lists.listsInMethods.enable()
        _ <- generator.console.consoleInMethods.enable()
        _ <- generator.arrays.arraysInMethods.enable()
        _ <- generator.equality.equalsInMethods.enable()
        _ <- generator.assertions.assertionsInMethods.enable()
        _ <- generator.booleans.booleansInMethodsInMethods.enable()

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


