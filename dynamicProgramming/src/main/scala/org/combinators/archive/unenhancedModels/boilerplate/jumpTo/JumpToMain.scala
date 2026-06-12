package org.combinators.archive.unenhancedModels.boilerplate.jumpTo

/**
 * An early implementation with unenhanced model.
 *
 * Test Cases does not compile, because this early implementation had not yet had available the code to properly create arrays
 * on which to integrate in a test case.
 *
 * val targetDirectory = Paths.get("target", "jumpto")
 */
import cats.effect.{ExitCode, IO, IOApp}
import com.github.javaparser.ast.PackageDeclaration
import org.apache.commons.io.FileUtils
import org.combinators.cogen.FileWithPathPersistable.*
import org.combinators.cogen.{FileWithPath, FileWithPathPersistable}
import org.combinators.dp.original.{BottomUp, GenerationOption, TopDown}
import org.combinators.ep.language.java.paradigm.ObjectOriented
import org.combinators.ep.language.java.{CodeGenerator, JavaNameProvider, Syntax, Unboxed}
import org.combinators.models.*
import org.combinators.models.original.Model

import java.nio.file.{Path, Paths}


class JumpToMainJava {
  val generator = CodeGenerator(CodeGenerator.defaultConfig.copy(boxLevel = Unboxed, targetPackage = new PackageDeclaration(ObjectOriented.fromComponents("dp"))))

  val dpApproach = JumpToMainProvider[Syntax.default.type, generator.paradigm.type](generator.paradigm)(JavaNameProvider, generator.imperativeInMethod, generator.doublesInMethod, generator.realDoublesInMethod, generator.consoleInMethod, generator.arraysInMethod, generator.assertionsInMethod, generator.stringsInMethod, generator.equalityInMethod, generator.ooParadigm, generator.parametricPolymorphism, generator.booleansInMethod)(generator.generics)

  val persistable = FileWithPathPersistable[FileWithPath]

  def directToDiskTransaction(targetDirectory: Path, model:Model, option:GenerationOption): IO[Unit] = {

    val files =
      () => generator.paradigm.runGenerator {
        for {
          _ <- generator.doublesInMethod.enable()
          _ <- generator.intsInMethod.enable()
          _ <- generator.stringsInMethod.enable()
          _ <- generator.listsInMethod.enable()
          _ <- generator.consoleInMethod.enable()
          _ <- generator.arraysInMethod.enable()
          _ <- generator.equalityInMethod.enable()
          _ <- generator.assertionsInMethod.enable()
          _ <- generator.booleansInMethod.enable()

          _ <- dpApproach.implement(model, option)
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

object JumpToMainDirectToDiskMain extends IOApp {
  val targetDirectory:Path = Paths.get("target", "jumpto")

  def run(args: List[String]): IO[ExitCode] = {

    val zero: LiteralInt = LiteralInt(0)
    val one: LiteralInt = LiteralInt(1)
    val ascii_zero:LiteralChar = LiteralChar('0')
    val two: LiteralInt = LiteralInt(2)

    // what was passed into constructor of the original class
    val input:InputExpression = InputExpression("array")   // might also need to pass in "type"

    val bound = List(ArgExpression(0, "array", IntegerArrayType(), "i"))

    val n: IteratorExpression = IteratorExpression(0, "i")   // only one argument, n

    val JumpTo = new Model("JumpTo",
      bound,
      cases = List(
        // array.length()-1 < n
        ( Some(ArrayLengthExpression(input) - one < n),  zero),
        // array.length()-1 = n
        ( Some(ArrayLengthExpression(input) - one == n),                        zero),
        // HACK == helper(n-1)
        ( None,                                                                 SubproblemExpression(Seq(n - one)))
      )
    )

    // choose one of these to pass in
    val topDown         = TopDown()
    val topDownWithMemo = TopDown(memo = true)
    val bottomUp        = BottomUp()

    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new JumpToMainJava() }
      _ <- IO { println("[OK]") }

      // pass in TOP DOWN
      result <- main.runDirectToDisc(targetDirectory, JumpTo, topDown)
    } yield result
  }
}
