package org.combinators.strings

/**
 * sbt "dp/runMain org.combinators.dp.DPJavaDirectToDiskMain"
 *
 * Creates output files in target/dp
 */
import cats.effect.{ExitCode, IO, IOApp}
import org.combinators.dp.enhanced.EnhancedDPMainJava
import org.combinators.dp.{BottomUp, TestExample, TopDown}
import org.combinators.model._

import java.nio.file.{Path, Paths}

/**
 * All that is needed here is the set of test cases that you need.
 */
class ThreeStringsLCSMainJava extends EnhancedDPMainJava {

  override def tests = Seq(
    new TestExample("ts1", new LiteralStringTriple("AGGT12", "12TXAYB", "12XBA"), new LiteralInt(2), new UnitExpression),
    new TestExample("ts2", new LiteralStringTriple("geeks", "geeksfor", "geeksforgeeks"), new LiteralInt(5), new UnitExpression),
    new TestExample("ts3", new LiteralStringTriple("abcd1e2", "bc12ea", "bd1ea"), new LiteralInt(3), new UnitExpression),
  )
}

object ThreeStringsLCSToDiskMain extends IOApp {
  val targetDirectory:Path = Paths.get("target", "dp")

  def model:EnhancedModel = {
    // Needed for conditions and fib(n-1) and fib(n-2)
    val zero: LiteralInt = new LiteralInt(0)
    val one: LiteralInt = new LiteralInt(1)

    // MatrixChainMultiplication has an array of N+1 integers,representing N 2D Matrices
    val s1  = new ArgExpression(0, "s1", new StringType(), "i")
    val s2  = new ArgExpression(1, "s2", new StringType(), "j")
    val s3  = new ArgExpression(2, "s3", new StringType(), "k")
    val bounds = List(s1, s2, s3)

    // COULD be inferred from the ArgExpression list, but this lets us name variable to use in iterator
    val i: HelperExpression = new HelperExpression("i")
    val j: HelperExpression = new HelperExpression("j")
    val k: HelperExpression = new HelperExpression("k")

    // what the compute() method calls with helper(s1.length(), s2.length())
    val symTable = Map("i" -> new StringLengthExpression(s1),
      "j" -> new StringLengthExpression(s2),
      "k" -> new StringLengthExpression(s3)
    )
    val sol = new SubproblemInvocation(symTable, returnType = new IntegerType())

    /*
     *   P(i,j,k) = 0, if i == 0 || j == 0 || k == 0 for all Ranges
     *   P(i,j,k) = Max of three sub-cases
     */

    val recursive_case = new MaxExpression(new SubproblemExpression(Seq(i - one, j, k)),
                                   new MaxExpression(new SubproblemExpression(Seq(i, j - one, k)),
                                                     new SubproblemExpression(Seq(i, j, k - one))))

    val strings_case = IfThenElseDefinition(new EqualExpression(new CharAtExpression(s1, i - one), new CharAtExpression(s2, j - one), new CharType) &&
                                                    new EqualExpression(new CharAtExpression(s2, j - one), new CharAtExpression(s3, k - one), new CharType),
      ExpressionStatement(new SubproblemExpression(Seq(i - one, j - one, k - one)) + one),
      ExpressionDefinition(recursive_case))

    val tslcs_definition = IfThenElseDefinition(i == zero || j == zero || k == zero, ExpressionStatement(zero),
     strings_case)

    val TSLCS = new EnhancedModel("ThreeStringLCS",
      bounds,
      subproblemType = new LiteralInt(0),         // helper() method returns int
      solutionType = new LiteralString(""),       // solution is a string, showing where characters come from S1 with parens
      sol,
      tslcs_definition)

    TSLCS
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
      topDownWithMemo
    }

    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new ThreeStringsLCSMainJava() }
      _ <- IO { println("[OK]") }

      result <- main.runDirectToDisc(targetDirectory, model, choice)
    } yield result
  }
}
