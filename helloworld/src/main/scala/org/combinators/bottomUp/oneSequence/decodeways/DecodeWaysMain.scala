package org.combinators.bottomUp.oneSequence.decodeways

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
import org.combinators.model.{ArgExpression, CharAtExpression, EqualExpression, InputExpression, IteratorExpression, LiteralChar, LiteralInt, Model, StringLengthExpression, StringType, SubproblemExpression, SubtractionExpression}

import java.nio.file.{Path, Paths}

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 */
class DPMainJava {
  val generator = CodeGenerator(CodeGenerator.defaultConfig.copy(boxLevel = PartiallyBoxed, targetPackage = new PackageDeclaration(ObjectOriented.fromComponents("dp"))))

  val dpApproach = DecodeWaysProvider[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.imperativeInMethod, generator.doublesInMethod, generator.realDoublesInMethod, generator.consoleInMethod, generator.arraysInMethod, generator.assertionsInMethod, generator.stringsInMethod, generator.equalityInMethod, generator.ooParadigm, generator.parametricPolymorphism)(generator.generics)

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

import java.util.Arrays;

class Solution {
    // Memoization table initialized with -1
    private int[] memo;

    public int numDecodings(String s) {
        if (s == null || s.length() == 0 || s.charAt(0) == '0') {
            return 0;
        }
        memo = new int[s.length()];
        // Initialize memo array with -1 to indicate uncomputed states
        Arrays.fill(memo, -1);
        return decode(s, 0);
    }

    private int decode(String s, int index) {
        // Base case: If we reach the end of the string, we have found one valid decoding
        if (index == s.length()) {
            return 1;
        }
        // If the current character is '0', it cannot be a valid single digit and cannot
        // start a two-digit number if it's the first digit, so return 0 ways
        if (s.charAt(index) == '0') {
            return 0;
        }
        // If the result for the current index is already computed, return it
        if (memo[index] != -1) {
            return memo[index];
        }

        int ways = 0;
        // Option 1: Decode a single digit
        ways += decode(s, index + 1);

        // Option 2: Decode a two-digit number if it is valid (between 10 and 26 inclusive)
        if (index + 1 < s.length()) {
            int twoDigit = Integer.parseInt(s.substring(index, index + 2));
            if (twoDigit >= 10 && twoDigit <= 26) {
                ways += decode(s, index + 2);
            }
        }

        // Store the result in the memo table before returning
        memo[index] = ways;
        return ways;
    }
}


 */

object DPDirectToDiskMain extends IOApp {
  val targetDirectory:Path = Paths.get("target", "decodeways")

  def run(args: List[String]): IO[ExitCode] = {

    val zero: LiteralInt = new LiteralInt(0)
    val one: LiteralInt = new LiteralInt(1)
    val ascii_zero:LiteralChar = new LiteralChar('0')
    val two: LiteralInt = new LiteralInt(2)

    // what was passed into constructor of the original class
    val input:InputExpression = new InputExpression("s")   // might also need to pass in "type"

    // for LCS

    // val text1:InputExpression("text1")
    // val text2:InputExpression("text2")

    val bound = List(new ArgExpression(0, "text1", new StringType(), "r"), new ArgExpression(1, "text2", new StringType(), "c"))

    val r: IteratorExpression = new IteratorExpression(0, "r")   // only one argument, n
    val c: IteratorExpression = new IteratorExpression(1, "c")   // only one argument, n

    val im1 = new SubtractionExpression(r, one)
    val im2 = new SubtractionExpression(c, two)

    val DecodeWays = new Model("DecodeWays",
      bound,
      cases = List(
        // s.length() == n
        ( Some(new EqualExpression(new StringLengthExpression(input), one)),  one ),
        // s.CharAt(n) == '0')
        ( Some(new EqualExpression(new CharAtExpression(input, one), ascii_zero)), zero),
        // HACK == helper(n-1)
        ( None,                                new SubproblemExpression(Seq(im1)))
      )
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
      result <- main.runDirectToDisc(targetDirectory, DecodeWays, topDown)
    } yield result
  }
}
