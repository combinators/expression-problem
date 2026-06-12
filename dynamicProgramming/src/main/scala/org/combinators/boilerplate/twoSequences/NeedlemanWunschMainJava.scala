package org.combinators.boilerplate.twoSequences

import cats.effect.{ExitCode, IO, IOApp}
import com.github.javaparser.ast.PackageDeclaration
import org.apache.commons.io.FileUtils
import org.combinators.dp.TestExample
import org.combinators.cogen.{FileWithPath, FileWithPathPersistable}
import FileWithPathPersistable.*
import org.combinators.dp.original.{BottomUp, GenerationOption, TopDown}
import org.combinators.ep.language.java.paradigm.ObjectOriented
import org.combinators.ep.language.java.{CodeGenerator, JavaNameProvider, Syntax, Unboxed}
import org.combinators.models.*
import org.combinators.models.enhancedModels.twoSequences.{NeedlemanWunschSequenceAlignment, WordBreak}

import java.nio.file.{Path, Paths}

// needs custom-support code, because the test case has unusual structure.
case class NeedlemanWunschSequenceInputType() extends ArgumentType
class NeedlemanWunschSequenceInput(val string1:String, val string2:String, val matchBonus:Int, val mismatchPenalty:Int, val gapPenalty:Int) extends LiteralExpression {
  def tpe:ArgumentType = NeedlemanWunschSequenceInputType()
}

class NeedlemanWunschMainJava {
  val generator = CodeGenerator(CodeGenerator.defaultConfig.copy(boxLevel = Unboxed, targetPackage = new PackageDeclaration(ObjectOriented.fromComponents("dp"))))

  val dpApproach = NeedlemanWunschSequenceAlignmentProvider[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.imperativeInMethod, generator.ooParadigm, generator.doublesInMethod, generator.realDoublesInMethod, generator.consoleInMethod, generator.arraysInMethod, generator.mapsInMethod, ???, generator.assertionsInMethod, generator.stringsInMethod, generator.equalityInMethod, generator.parametricPolymorphism, generator.booleansInMethod)(generator.generics)

  val persistable = FileWithPathPersistable[FileWithPath]

  // subclasses will provide tests and model
  val tests = Seq(
    new TestExample("nws1", new NeedlemanWunschSequenceInput("abc", "ace", +2, -1, -2), LiteralInt(0), new UnitExpression),
    new TestExample("nws2", new NeedlemanWunschSequenceInput("CTCGCAGC", "CATTCAC", +10, -2, -5), LiteralInt(33), new UnitExpression),
  )

  val model: EnhancedModel = new NeedlemanWunschSequenceAlignment().model

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

object NeedlemanWunschDirectToDiskMain extends IOApp {
  val targetDirectory:Path = Paths.get("target", "dp", "needlemanWunsch")

  def run(args: List[String]): IO[ExitCode] = {

    // choose one of these to pass in
    val topDown         = TopDown()
    val topDownWithMemo = TopDown(memo = true)
    val bottomUp        = BottomUp()

    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new NeedlemanWunschMainJava() }
      _ <- IO { println("[OK]") }

      result <- main.runDirectToDisc(targetDirectory, bottomUp)
    } yield result
  }
}