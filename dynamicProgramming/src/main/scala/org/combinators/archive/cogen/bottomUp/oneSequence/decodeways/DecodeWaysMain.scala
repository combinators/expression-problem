package org.combinators.archive.cogen.bottomUp.oneSequence.decodeways

/**
 * An early implementation with unenhanced model that never was completed.
 *
 * Generated DP solution does not work and test cases remain connected to Fibonacci.
 *
 * val targetDirectory = Paths.get("target", "decodeways")
 */

import cats.effect.{ExitCode, IO, IOApp}
import com.github.javaparser.ast.PackageDeclaration
import org.apache.commons.io.FileUtils
import org.combinators.cogen.{FileWithPath, FileWithPathPersistable}
import FileWithPathPersistable._
import org.combinators.dp.original.{BottomUp, GenerationOption, TopDown}
import org.combinators.ep.language.java.paradigm.ObjectOriented
import org.combinators.ep.language.java.{CodeGenerator, JavaNameProvider, Unboxed, Syntax}
import org.combinators.models._
import org.combinators.models.original.Model
import java.nio.file.{Path, Paths}

/**
 * Eventually encode a set of subclasses/traits to be able to easily specify (a) the variation; and (b) the evolution.
 */
class DPMainJava {
  val generator = CodeGenerator(CodeGenerator.defaultConfig.copy(boxLevel = Unboxed, targetPackage = new PackageDeclaration(ObjectOriented.fromComponents("dp"))))

  val dpApproach = DecodeWaysProvider[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.imperativeInMethod, generator.doublesInMethod, generator.realDoublesInMethod, generator.consoleInMethod, generator.arraysInMethod, generator.assertionsInMethod, generator.stringsInMethod, generator.equalityInMethod, generator.ooParadigm, generator.parametricPolymorphism, generator.booleansInMethod)(generator.generics)

  val persistable = FileWithPathPersistable[FileWithPath]

  def directToDiskTransaction(targetDirectory: Path, model:Model, option:GenerationOption): IO[Unit] = {

    val files =
      () => generator.paradigm.runGenerator {
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

@deprecated(message = "Generated code in Java does not compile.")
object DPDirectToDiskMain extends IOApp {
  val targetDirectory:Path = Paths.get("target", "decodeways")

  def run(args: List[String]): IO[ExitCode] = {

    val zero: LiteralInt = LiteralInt(0)
    val one: LiteralInt = LiteralInt(1)
    val ascii_zero:LiteralChar = LiteralChar('0')
    val two: LiteralInt = LiteralInt(2)

    // what was passed into constructor of the original class
    val input:InputExpression = InputExpression("s")   // might also need to pass in "type"

    val bound = List(ArgExpression(0, "text1", StringType(), "r"), ArgExpression(1, "text2", StringType(), "c"))

    val r: IteratorExpression = IteratorExpression(0, "r")   // only one argument, n
    val c: IteratorExpression = IteratorExpression(1, "c")   // only one argument, n

    val DecodeWays = new Model("DecodeWays",
      bound,
      cases = List(
        // s.length() == n
        ( Some(StringLengthExpression(input) == one),       one ),
        // s.CharAt(n) == '0')
        ( Some(CharAtExpression(input, one) == ascii_zero), zero),
        // HACK == helper(n-1)
        ( None,                                             SubproblemExpression(Seq(r - one)))
      )
    )

    // choose one of these to pass in
    val topDown         = TopDown()
    val topDownWithMemo = TopDown(memo = true)
    val bottomUp        = BottomUp()

    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new DPMainJava() }
      _ <- IO { println("[OK]") }

      // pass in TOP DOWN
      result <- main.runDirectToDisc(targetDirectory, DecodeWays, topDown)
    } yield result
  }
}
