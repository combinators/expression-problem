package org.combinators.oneSequence

import cats.effect.{ExitCode, IO, IOApp}
import org.combinators.dp.enhanced.EnhancedDPMainJava
import org.combinators.dp.{BottomUp, TestExample, TopDown}
import org.combinators.model.{ArrayElementExpression, SubproblemExpression, _}

import java.nio.file.{Path, Paths}

/**
 * All that is needed here is the set of test cases that you need.
 */
class MinCostClimbingStairMain extends EnhancedDPMainJava {

  override def tests = Seq(
    new TestExample("ts1", new LiteralArray(Array(1,100,1,1,1,100,1,1,100,1)), new LiteralInt(6), new UnitExpression),
    new TestExample("ts2", new LiteralArray(Array(10,15,20)), new LiteralInt(15), new UnitExpression),
  )
}

object MinCostClimbingStairToDiskMain extends IOApp {
  val targetDirectory:Path = Paths.get("target", "dp")

  def model:EnhancedModel = {
    val zero: LiteralInt = new LiteralInt(0)
    val one: LiteralInt = new LiteralInt(1)
    val two: LiteralInt = new LiteralInt(2)

    val costs  = new ArgExpression(0, "costs", IntegerArrayType(), "i")
    val bounds = List(costs)

    // COULD be inferred from the ArgExpression list, but this lets us name variable to use in iterator
    val i: HelperExpression = HelperExpression("i", zero, SelfExpression("i") < new ArrayLengthExpression(costs), new ArrayLengthExpression(costs))

    // what the compute() method calls with helper(s1.length(), s2.length())
    val helpers = Map("i" -> i)
    val sol = SubproblemInvocation(order=Seq("i"), helpers = helpers, returnType = IntegerType())

    /*
     *   P(i,j,k) = 0, if i == 0 || j == 0 || k == 0 for all Ranges
     *   P(i,j,k) = Max of three sub-cases
     */

    // return cost[i] + Math.min(helper_topdown(i - 1),
    //                helper_topdown(i - 2));
    val recursive_case = new ArrayElementExpression(costs, i) + new MinExpression(new SubproblemExpression(Seq(i - one)), new SubproblemExpression(Seq(i - two)))

// if (i == 0 || i == 1) {
//            return cost[i];
//        }
    val mccs_definition = IfThenElseDefinition((i == zero) || (i == one),
      ExpressionStatement(new ArrayElementExpression(costs, i)),
      ExpressionDefinition(recursive_case))


    val MCCS = new EnhancedModel("MinCostClimbingStair",
      bounds,
      subproblemType = IntegerType(),         // helper() method returns int
      solutionType   = StringType(),          // solution is a string, showing where characters come from S1 with parens
      sol,
      mccs_definition,

      // how to determine answer
      answer = new TernaryExpression(new ArrayLengthExpression(costs) == one,
        new ArrayElementExpression(costs, zero),
        new MinExpression(
          new SubproblemExpression(Seq(new ArrayLengthExpression(costs) - one)),
          new SubproblemExpression(Seq(new ArrayLengthExpression(costs) - two))))
    )

    MCCS
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
      main <- IO { new MinCostClimbingStairMain() }
      _ <- IO { println("[OK]") }

      result <- main.runDirectToDisc(targetDirectory, model, choice)
    } yield result
  }
}
