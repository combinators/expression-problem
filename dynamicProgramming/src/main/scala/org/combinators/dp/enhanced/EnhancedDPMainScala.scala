package org.combinators.dp.enhanced

import cats.effect.{ExitCode, IO, IOApp}
import org.apache.commons.io.FileUtils
import org.combinators.ep.language.scala.ast.ffi.*
import org.combinators.ep.language.scala.ast.{FinalBaseAST, FinalNameProviderAST}
import org.combinators.ep.language.scala.codegen.CodeGenerator
import org.combinators.ep.language.scala.codegen.FullAST
import org.combinators.cogen.*
import org.combinators.cogen.FileWithPathPersistable
import org.combinators.cogen.FileWithPathPersistable.fileWithPathPersistable
import org.combinators.dp.original.{BottomUp, GenerationOption, TopDown}
import org.combinators.dp.TestExample
import org.combinators.models.EnhancedModel

import java.nio.file.{Path, Paths}

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 */
abstract class EnhancedDPMainScala extends IOApp with EnhancedMainInterface {
  val _ast: FullAST = new FinalBaseAST
    with FinalNameProviderAST
    with FinalArithmeticAST
    with FinalArraysAST
    with FinalAssertionsAST
    with FinalBooleanAST
    with FinalConsoleAST
    with FinalExceptionsAST
    with FinalEqualsAST
    with FinalListsAST
    with FinalMapsAST
    with FinalOperatorExpressionsAST
    with FinalRealArithmeticOpsAST
    with FinalStringAST {
    val reificationExtensions = List.empty
  }
  val generator: CodeGenerator[_ast.type] = CodeGenerator("dp", _ast, Set.empty)

  val dpApproach = EnhancedDPObjectOrientedProvider[generator.syntax.type, generator.paradigm.type](generator.paradigm)(generator.nameProvider, generator.imperative.imperativeInMethods,  generator.ooParadigm, generator.doubles.arithmeticInMethods, generator.realDoubles.realArithmeticInMethods, generator.console.consoleInMethods, generator.arrays.arraysInMethods, generator.maps.mapsInMethods, generator.maps.mapsIn[generator.ooParadigm.ConstructorContext], generator.assertions.assertionsInMethods, generator.strings.stringsInMethods, generator.equality.equalsInMethods, generator.parametricPolymorphism, generator.booleans.booleansInMethodsInMethods)(generator.generics)

  val persistable = FileWithPathPersistable[FileWithPath]

  // subclasses will provide tests and model
  def tests:Seq[TestExample]
  def model:EnhancedModel
  
  // subclasses describe how to instantiate desired app class
  def constructApp(): EnhancedDPMainScala

  def filesToGenerate(option: GenerationOption): Seq[FileWithPath] = {
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
        _ <- generator.maps.mapsInMethods.enable()
        _ <- generator.maps.mapsIn[generator.ooParadigm.ConstructorContext].enable()

        // HERE you can finally specify the method to use for testing and the test cases
        _ <- dpApproach.implement(model, tests, option)
      } yield ()
    }
  }

  def directToDiskTransaction(targetDirectory: Path, option:GenerationOption): IO[Unit] = {

     IO {
      print("Computing Files...")
      val computed = filesToGenerate(option)
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

  def runDirectToDisc(targetDirectory: Path, option:GenerationOption): IO[ExitCode] = {
    for {
      _ <- directToDiskTransaction(targetDirectory, option)
    } yield ExitCode.Success
  }

  def run(args: List[String]): IO[ExitCode] = {

    // choose one of these to pass in
    val topDown = TopDown()
    val topDownWithMemo = TopDown(memo = true)
    val bottomUp = BottomUp()

    val choice = if (args.length == 1) {
      args(0).toLowerCase() match {
        case "topdown" => topDown
        case "topdownwithmemo" => topDownWithMemo
        case "topdownmemo" => topDownWithMemo
        case "bottomup" => bottomUp
        case _ => ???
      }
    } else {
      bottomUp
    }

    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { constructApp() }
      _ <- IO { println("[OK]") }

      result <- main.runDirectToDisc(Paths.get("target", "scala", model.problem), choice)
    } yield result
  }
}


