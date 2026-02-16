package org.combinators.integer

/**
 * sbt "dp/runMain org.combinators.dp.DPJavaDirectToDiskMain"
 *
 * Creates output files in target/dp
 */

import cats.effect.{ExitCode, IO, IOApp}
import com.github.javaparser.ast.PackageDeclaration
import org.apache.commons.io.FileUtils
import org.combinators.dp.enhanced.{EnhancedDPMainJava, EnhancedDPObjectOrientedProvider}
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
class DiceThrowMainJava extends EnhancedDPMainJava  {

  val tests = Seq(
    new TestExample("dt1", new LiteralTriple(6, 3, 12), new LiteralInt(25), new UnitExpression) //  https://www.geeksforgeeks.org/dsa/dice-throw-dp-30/
  )
}

object DiceThrowDirectToDiskMain extends IOApp {
  val targetDirectory:Path = Paths.get("target", "dp")

  /**
   * Name: Dice Throw
   * Description:
   * Given n dices each with m faces, numbered from 1 to m, find the number of ways to get sum x,
   * which is the summation of values on each face
   *
   * Example:
   * m=2, n=3, x=6
   * there is only 1 way to get the sum 6 using 3 dices from 1 to 2
   */

  def model: EnhancedModel = {
    val zero: LiteralInt = new LiteralInt(0)
    val one: LiteralInt = new LiteralInt(1)

    val m = new ArgExpression(2, "m", IntegerType(), "m")    // m never changes and is not part of helper
    val n = new ArgExpression(0, "n", IntegerType(), "i")    // not sure if 'i' is used
    val x = new ArgExpression(1, "x", IntegerType(), "j")

    val bound_ps = List(m, n, x)

    val i: HelperExpression = HelperExpression("i", zero, SelfExpression("i") <= n, n + one)   // need to be zero for BottomUp to be able to trigger base case within loop.
    val j: HelperExpression = HelperExpression("j", zero, SelfExpression("j") <= x, x + one)

    val k: HelperExpression = HelperExpression("k", i, SelfExpression("k") <= m, m)            // in_range is not essential since this is not an argument to helper/subproblem

    val additiveExpression:Expression = new SubproblemExpression(Seq(i - one, j - k))
    val k_sum = SumDefinition("k", one, k <= m && zero <= j - k, additiveExpression, k + one)

    val helperTable = Map("i" -> i, "j" -> j, "k" -> k)
    val sol_dt = SubproblemInvocation(Seq("i", "j"), helpers = helperTable)

    val base2 = IfThenElseDefinition(i == zero || j <= zero, ExpressionStatement(zero), k_sum)
    val dt_definition = IfThenElseDefinition(i == zero && j == zero, ExpressionStatement(one), base2)

    val DiceThrow = new EnhancedModel("DiceThrow",
      bound_ps,
      subproblemType = IntegerType(),    // helper method is an int
      solutionType   = StringType(),     // solution is a string
      sol_dt,
      dt_definition,
      answer = new SubproblemExpression(Seq(n, x))
    )

    DiceThrow
  }

  def run(args: List[String]): IO[ExitCode] = {

    // choose one of these to pass in
    val topDown         = TopDown()
    val topDownWithMemo = TopDown(memo = true)
    val bottomUp        = BottomUp()

    val choice = if (args.length == 1) {
      args(0).toLowerCase() match {
        case "topdown" => topDown
        case "topdownwithmemo" => topDownWithMemo
        case "bottomUp" => bottomUp
        case _ => ???
      }
    } else {
      bottomUp
    }

    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new DiceThrowMainJava() }
      _ <- IO { println("[OK]") }

      result <- main.runDirectToDisc(targetDirectory, DiceThrowDirectToDiskMain.model, choice)
    } yield result
  }
}
