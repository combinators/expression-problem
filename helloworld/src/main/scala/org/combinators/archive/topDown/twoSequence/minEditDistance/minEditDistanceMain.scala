package org.combinators.archive.topDown.twoSequence.minEditDistance

/**
 * sbt "dp/runMain org.combinators.dp.DPJavaDirectToDiskMain"
 *
 * Creates output files in target/dp
 */

import cats.effect.{ExitCode, IO, IOApp}
import com.github.javaparser.ast.PackageDeclaration
import org.apache.commons.io.FileUtils
import org.combinators.dp.{BottomUp, DPObjectOrientedProvider, GenerationOption, TestExample, TopDown, TopDownStrategy}
import org.combinators.ep.generator.FileWithPathPersistable._
import org.combinators.ep.generator.{FileWithPath, FileWithPathPersistable}
import org.combinators.ep.language.java.paradigm.ObjectOriented
import org.combinators.ep.language.java.{CodeGenerator, JavaNameProvider, PartiallyBoxed, Syntax}
import org.combinators.model.{ArgExpression, CharAtExpression, EqualExpression, InputExpression, IteratorExpression, LiteralChar, LiteralInt, Model, StringLengthExpression, StringType, SubproblemExpression, SubtractionExpression, MinExpression}

import java.nio.file.{Path, Paths}

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 */
class DPMainJava {
  val generator = CodeGenerator(CodeGenerator.defaultConfig.copy(boxLevel = PartiallyBoxed, targetPackage = new PackageDeclaration(ObjectOriented.fromComponents("dp"))))

  val dpApproach = minEditDistanceProvider[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.imperativeInMethod, generator.doublesInMethod, generator.realDoublesInMethod, generator.consoleInMethod, generator.arraysInMethod, generator.assertionsInMethod, generator.stringsInMethod, generator.equalityInMethod, generator.ooParadigm, generator.parametricPolymorphism, generator.booleansInMethod)(generator.generics)

  val persistable = FileWithPathPersistable[FileWithPath]

  def directToDiskTransaction(targetDirectory: Path, model:Model, option:GenerationOption): IO[Unit] = {

    val files =
      () => generator.paradigm.runGenerator {
        for {
          _ <- generator.doublesInMethod.enable()
          _ <- generator.realDoublesInMethod.enable()
          _ <- generator.intsInMethod.enable()
          _ <- generator.stringsInMethod.enable()
          _ <- generator.listsInMethod.enable()     // should be array, but this still needs to be added as an FFI
          _ <- generator.consoleInMethod.enable()
          _ <- generator.arraysInMethod.enable()
          _ <- generator.equalityInMethod.enable()
          _ <- generator.assertionsInMethod.enable()
          _ <- generator.booleansInMethod.enable()

          _ <- dpApproach.implement(model, option)   // WRONG METHODs
        } yield ()
      }

    IO {
      print("Computing Files...")
      val computed = files()
      println("[OK]")
      if (targetDirectory.toFile.exists()) {
        print(s"Cleaning Target Directory ($targetDirectory)...")
        FileUtils.deleteDirectory(targetDirectory.toFile)
        println("[OK]")
      }
      print("Persisting Files...")
      files().foreach(file => persistable.persistOverwriting(targetDirectory, file))
      println("[OK]")
    }
  }

  def runDirectToDisc(targetDirectory: Path, model:Model, option:GenerationOption): IO[ExitCode] = {
    for {
      _ <- directToDiskTransaction(targetDirectory, model, option)
    } yield ExitCode.Success
  }
}

/*
function editDistRec(s1, s2, m, n) {

    if (m === 0) return n;

    if (n === 0) return m;

    if (s1[m - 1] === s2[n - 1])
        return editDistRec(s1, s2, m - 1, n - 1);

    return 1 + Math.min(
               editDistRec(s1, s2, m, n - 1),
               editDistRec(s1, s2, m - 1, n),
               editDistRec(s1, s2, m - 1, n - 1));
}

function editDistance(s1, s2) {
    return editDistRec(s1, s2, s1.length, s2.length);
}
 */

object DPDirectToDiskMain extends IOApp {
  val targetDirectory:Path = Paths.get("target", "minEditDistance")

  def run(args: List[String]): IO[ExitCode] = {

    val zero: LiteralInt = new LiteralInt(0)
    val one: LiteralInt = new LiteralInt(1)

    // what was passed into constructor of the original class
    val string1:InputExpression = new InputExpression("s1")
    val string2:InputExpression = new InputExpression("s2")

    val bound = List(new ArgExpression(0, "s1", new StringType(), "m"), new ArgExpression(1, "s2", new StringType(), "n"))

    val m: IteratorExpression = new IteratorExpression(0, "m")   // only one argument, n
    val n: IteratorExpression = new IteratorExpression(1, "n")   // only one argument, n

    val im1 = new SubtractionExpression(m, one)
    val in1 = new SubtractionExpression(n, one)

    val minEditDistance = new Model("MinEditDistance",
      bound,
      cases = List(
        // if (m === 0) return n;
        ( Some(new EqualExpression(m, zero)),  n ),
        // if (n === 0) return m;
        ( Some(new EqualExpression(n, zero)), m),
        //if (s1[m - 1] === s2[n - 1]) return editDistRec(s1, s2, m - 1, n - 1);
        ( Some(new EqualExpression(new CharAtExpression(string1, im1), new CharAtExpression(string2, in1))), new SubproblemExpression(Seq(im1, in1))),
        //return 1 + Math.min(
          //editDistRec(s1, s2, m, n - 1),
          //editDistRec(s1, s2, m - 1, n),
          //editDistRec(s1, s2, m - 1, n - 1));
        ( None,new MinExpression(new SubproblemExpression(Seq(m, in1)),
          new MinExpression(new SubproblemExpression(Seq(im1, n)),
          new SubproblemExpression(Seq(im1, in1))))
      ))
    )

    // choose one of these to pass in
    val topDown         = new TopDown()
    val topDownWithMemo = new TopDown(memo = true)
    val bottomUp        = new BottomUp()

    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new DPMainJava() }
      _ <- IO { println("[OK]") }

      // pass in TOP DOWN
      result <- main.runDirectToDisc(targetDirectory, minEditDistance, topDown)
    } yield result
  }
}
