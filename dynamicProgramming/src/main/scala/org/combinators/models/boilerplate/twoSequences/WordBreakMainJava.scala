package org.combinators.models.boilerplate.twoSequences

import cats.effect.{ExitCode, IO, IOApp}
import com.github.javaparser.ast.PackageDeclaration
import org.apache.commons.io.FileUtils
import org.combinators.dp.enhanced.EnhancedMainInterface
import org.combinators.dp.{BottomUp, GenerationOption, TestExample, TopDown}
import org.combinators.models._
import org.combinators.cogen.{FileWithPath, FileWithPathPersistable}
import FileWithPathPersistable._
import org.combinators.ep.language.java.paradigm.ObjectOriented
import org.combinators.ep.language.java.{CodeGenerator, JavaNameProvider, Syntax, Unboxed}

import org.combinators.models.enhancedModels.twoSequences.WordBreak

import java.nio.file.{Path, Paths}

// needs custom-support code, because the test case has unusual structure.
case class WordBreakInputType() extends ArgumentType
class WordBreakInput(val s:String, val dictionary:Array[String]) extends LiteralExpression {
  def tpe:ArgumentType = WordBreakInputType()
}

class WordBreakMainJava extends EnhancedMainInterface {

  def tests = Seq(
    // https://rna.informatik.uni-freiburg.de/Teaching/index.jsp?toolName=Needleman-Wunsch has really nice example
    // from google search via AI so cannot trace,
    new TestExample("wb1", new WordBreakInput("catsanddog", Array("cats","dog","sand","and","cat")), new LiteralBoolean(false), new UnitExpression),
    new TestExample("wb2", new WordBreakInput("leetcode", Array("leet","code")), new LiteralBoolean(true), new UnitExpression)
    // https://medium.com/@nandiniumbarkar/needleman-wunsch-algorithm-7bba68b510db
  )

  val generator = CodeGenerator(CodeGenerator.defaultConfig.copy(boxLevel = Unboxed, targetPackage = new PackageDeclaration(ObjectOriented.fromComponents("dp"))))

  val dpApproach = WordBreakProvider[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.imperativeInMethod, generator.doublesInMethod, generator.realDoublesInMethod, generator.consoleInMethod, generator.arraysInMethod, generator.assertionsInMethod, generator.stringsInMethod, generator.equalityInMethod, generator.ooParadigm, generator.parametricPolymorphism, generator.booleansInMethod)(generator.generics)

  val persistable = FileWithPathPersistable[FileWithPath]

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

object WordBreakToDiskMain extends IOApp {
  val targetDirectory: Path = Paths.get("target", "dp", "wordbreak")

  val model: EnhancedModel = new WordBreak().model

  def run(args: List[String]): IO[ExitCode] = {
    val topDown = TopDown()
    val topDownWithMemo = TopDown(memo = true)
    val bottomUp = BottomUp()

    val choice = if (args.length == 1) {
      args(0).toLowerCase() match {
        case "topdown" => topDown
        case "topdownwithmemo" => topDownWithMemo
        case "bottomup" => bottomUp
        case _ => ???
      }
    } else {
      bottomUp
    }

    for {
      _ <- IO {
        print("Initializing Generator...")
      }
      main <- IO {
        new WordBreakMainJava()
      }
      _ <- IO {
        println("[OK]")
      }

      result <- main.runDirectToDisc(targetDirectory, model, choice)
    } yield result
  }
}

