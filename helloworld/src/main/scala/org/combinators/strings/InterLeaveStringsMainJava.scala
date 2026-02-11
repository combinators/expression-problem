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
class InterleaveStringsMainJava extends EnhancedDPMainJava {

  override def tests = Seq(
    new TestExample("ils1", new LiteralStringTriple("aabcc", "dbbca", "aadbbcbcac"), new LiteralBoolean(true), new UnitExpression),
    new TestExample("ils1", new LiteralStringTriple("aab", "axy", "aaxaby"), new LiteralBoolean(true), new UnitExpression),
    new TestExample("ils1", new LiteralStringTriple("aab", "axy", "abaaxy"), new LiteralBoolean(false), new UnitExpression),
  )
}

object InterleaveStringsToDiskMain extends IOApp {
  val targetDirectory:Path = Paths.get("target", "dp")

  def model:EnhancedModel = {
    // Needed for conditions and fib(n-1) and fib(n-2)
    val zero: LiteralInt = new LiteralInt(0)
    val one: LiteralInt = new LiteralInt(1)

    // MatrixChainMultiplication has an array of N+1 integers,representing N 2D Matrices
    val s1  = new ArgExpression(0, "s1", new StringType(), "i1")
    val s2  = new ArgExpression(1, "s2", new StringType(), "i2")
    val s3  = new ArgExpression(2, "s3", new StringType(), "i3")
    val bounds = List(s1, s2, s3)

    // COULD be inferred from the ArgExpression list, but this lets us name variable to use in iterator
    val i1: HelperExpression = new HelperExpression("i1")
    val i2: HelperExpression = new HelperExpression("i2")

    // what the compute() method calls with helper(s1.length(), s2.length())
    val symTable = Map("i1" -> new StringLengthExpression(s1), "i2" -> new StringLengthExpression(s2))
    val sol = new SubproblemInvocation(symTable, returnType = new BooleanType())

    /*
     *   P(i,j) = 0, if i == j
     *   P(i,j) = Min (k, P(i,k) + P(k+1,j) + cost of multiplying resulting two matrices)
     *      for (int k = i; k < j; k++)
     */

    val subprobExpr = new AdditionExpression(i1, i2) /// HACK HACK just to compile
//      new SubproblemExpression(Seq(i,k)),
//      new AdditionExpression(
//        new SubproblemExpression(Seq(new AdditionExpression(k, one), j)),
//        new MultiplicationExpression(new ArrayElementExpression(array, new SubtractionExpression(i, one)),
//          new MultiplicationExpression(new ArrayElementExpression(array, k), new ArrayElementExpression(array, j))
//        )
//      )
//    )

    // Min range definition for k in range from i (inclusive) to j (exclusive) with an advance of k+1
    val k: HelperExpression = new HelperExpression("k")

    val case_final = new OrExpression(
      new AndExpression(
        new EqualExpression(new CharAtExpression(s1, i1 - one), new CharAtExpression(s3, i1 + i2 - one), new CharType),   // EXTREMELY important to not overload and specify CHAR
        new SubproblemExpression(Seq(i1 - one, i2))
      ),
      new AndExpression(
        new EqualExpression(new CharAtExpression(s2, i2 - one), new CharAtExpression(s3, i1 + i2 - one), new CharType),   // EXTREMELY important to not overload and specify CHAR
        new SubproblemExpression(Seq(i1, i2 - one))
      )
    )

    val case_5 = new IfThenElseDefinition(i2 == zero,
      new ExpressionStatement(
        new EqualExpression(new CharAtExpression(s1, i1 - one), new CharAtExpression(s3, i1 - one), new CharType)         // EXTREMELY important to not overload and specify CHAR
      ),
      new ExpressionDefinition(case_final))

    val case_4 = new IfThenElseDefinition(i1 == zero,
      new ExpressionStatement(
        new EqualExpression(new CharAtExpression(s2, i2 - one), new CharAtExpression(s3, i2 - one), new CharType)         // EXTREMELY important to not overload and specify CHAR
      ),
      case_5)

    val case_3 = new IfThenElseDefinition((i1 == zero) && (i2==zero),
      new ExpressionStatement(new LiteralBoolean(true)),
      case_4)

    val case_2 = new IfThenElseDefinition(new StringLengthExpression(s3) < i1 + i2,
      new ExpressionStatement(new LiteralBoolean(false)),
      case_3)

    val case_1 = new IfThenElseDefinition(new StringLengthExpression(s2) < i2,
      new ExpressionStatement(new LiteralBoolean(false)),
      case_2)

    val ils_definition = new IfThenElseDefinition(new StringLengthExpression(s1) < i1,
      new ExpressionStatement(new LiteralBoolean(false)),
      case_1)

    val ILS = new EnhancedModel("InterleavingStrings",
      bounds,
      subproblemType = new LiteralBoolean(true),  // helper() method returns boolean
      solutionType = new LiteralString(""),       // solution is a string, showing where characters come from S1 with parens
      sol,
      ils_definition)

    ILS
  }

  def run(args: List[String]): IO[ExitCode] = {

    // choose one of these to pass in
    val topDown         = new TopDown()
    val topDownWithMemo = new TopDown(memo = true)
    val bottomUp        = new BottomUp()

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
      main <- IO { new InterleaveStringsMainJava() }
      _ <- IO { println("[OK]") }

      result <- main.runDirectToDisc(targetDirectory, model, choice)
    } yield result
  }
}
