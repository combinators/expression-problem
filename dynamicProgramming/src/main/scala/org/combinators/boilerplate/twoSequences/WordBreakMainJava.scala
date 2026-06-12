package org.combinators.boilerplate.twoSequences

import cats.effect.{ExitCode, IO, IOApp}
import com.github.javaparser.ast.PackageDeclaration
import org.apache.commons.io.FileUtils
import org.combinators.dp.TestExample
import org.combinators.cogen.{FileWithPath, FileWithPathPersistable}
import FileWithPathPersistable.*
import org.combinators.boilerplate.twoSequences.WordBreakProvider
import org.combinators.dp.original.{BottomUp, GenerationOption, TopDown}
import org.combinators.ep.language.java.paradigm.ObjectOriented
import org.combinators.ep.language.java.{CodeGenerator, JavaNameProvider, Syntax, Unboxed}
import org.combinators.models.*
import org.combinators.models.enhancedModels.twoSequences.WordBreak

import java.nio.file.{Path, Paths}

// needs custom-support code, because the test case has unusual structure.
case class WordBreakInputType() extends ArgumentType
class WordBreakInput(val s:String, val dictionary:Array[String]) extends LiteralExpression {
  def tpe:ArgumentType = WordBreakInputType()
}

class WordBreakMainJava {
  val generator = CodeGenerator(CodeGenerator.defaultConfig.copy(boxLevel = Unboxed, targetPackage = new PackageDeclaration(ObjectOriented.fromComponents("dp"))))

  val dpApproach = WordBreakProvider[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.imperativeInMethod, generator.doublesInMethod, generator.realDoublesInMethod, generator.consoleInMethod, generator.arraysInMethod, generator.mapsInMethod, generator.assertionsInMethod, generator.stringsInMethod, generator.equalityInMethod, generator.ooParadigm, generator.parametricPolymorphism, generator.booleansInMethod)(generator.generics)

  val persistable = FileWithPathPersistable[FileWithPath]

  // subclasses will provide tests and model
  val tests = Seq(
    new TestExample("wb1", new WordBreakInput("catsanddog", Array("cats","dog","sand","and","cat")), LiteralBoolean(false), new UnitExpression),
    new TestExample("wb2", new WordBreakInput("leetcode", Array("leet","code")), LiteralBoolean(true), new UnitExpression)
  )

  val model: EnhancedModel = new WordBreak().model

  def filesToGenerate(option: GenerationOption): Seq[FileWithPath] = {
    println(s"Generating ${model.problem}...")
    generator.paradigm.runGenerator {
      for {
        _ <- generator.doublesInMethod.enable()
        _ <- generator.realDoublesInMethod.enable()
        _ <- generator.intsInMethod.enable()
        _ <- generator.stringsInMethod.enable()
        _ <- generator.listsInMethod.enable()
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
}

object WordBreakDirectToDiskMain extends IOApp {
  val targetDirectory:Path = Paths.get("target", "dp", "wordBreak")

  def run(args: List[String]): IO[ExitCode] = {

    // choose one of these to pass in
    val topDown         = TopDown()
    val topDownWithMemo = TopDown(memo = true)
    val bottomUp        = BottomUp()

    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new WordBreakMainJava() }
      _ <- IO { println("[OK]") }

      result <- main.runDirectToDisc(targetDirectory, bottomUp)
    } yield result
  }
}