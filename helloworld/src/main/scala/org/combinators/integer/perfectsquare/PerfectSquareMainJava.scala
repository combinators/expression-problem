package org.combinators.integer.perfectsquare

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
class PerfectSquareMainJava extends EnhancedDPMainJava  {

  val tests = Seq(
    new TestExample("ps1", new LiteralInt(13), new LiteralInt(2), new UnitExpression), // 9 + 4
    new TestExample("ps2", new LiteralInt(14), new LiteralInt(3), new UnitExpression), // 9 + 4 + 1
    new TestExample("ps3", new LiteralInt(15), new LiteralInt(4), new UnitExpression), // 9 + 4 + 1 + 1
    new TestExample("ps4", new LiteralInt(16), new LiteralInt(1), new UnitExpression), // 16
  )

}

object PerfectSquareMainDirectToDiskMain extends IOApp {
  val targetDirectory:Path = Paths.get("target", "dp")

  def model: EnhancedModel = {
    // Needed for conditions and fib(n-1) and fib(n-2)
    val zero: LiteralInt = new LiteralInt(0)
    val one: LiteralInt = new LiteralInt(1)

    /* Perfect Square. */
    val n = new ArgExpression(0, "n", IntegerType(), "i")
    val bound_ps = List(n)

    // COULD be inferred from the ArgExpression list, but this lets us name variable to use in iterator
    val i: HelperExpression = new HelperExpression("i", one, SelfExpression("i") <= n)    // only one argument, i: not too sure this is right
    val k: HelperExpression = new HelperExpression("k", one, SelfExpression("k") * SelfExpression("k") <= n)    // not too sure this is right

    val parameters = Map("i" -> (n, i))           // what is the solution invocation?
    val helperTable_ps = Map("k" -> k)            // Boy this is awkward: need k as helper but not in invocation
    val sol_ps = SubproblemInvocation(parameters, Seq("i", "k"), helpers = helperTable_ps)

    val ps_subprobExpr = new AdditionExpression(one, new SubproblemExpression(Seq(i - k * k)))
    val def_ps = MinRangeDefinition(Seq(i), k, one, k * k < i + one, ps_subprobExpr, k + one)
    val ps_inner_definition =  IfThenElseDefinition(i == one, ExpressionStatement(one), def_ps)
    val ps_definition = IfThenElseDefinition(i == zero, ExpressionStatement(zero), ps_inner_definition)

    val PerfectSquare = new EnhancedModel("PerfectSquare",
      bound_ps,
      subproblemType = IntegerType(),    // helper method is an int
      solutionType   = StringType(), // solution is a string
      sol_ps,
      ps_definition
    )

    PerfectSquare
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
      topDown
    }

    for {
      _ <- IO { print("Initializing Generator...") }
      main <- IO { new PerfectSquareMainJava() }
      _ <- IO { println("[OK]") }

      result <- main.runDirectToDisc(targetDirectory, PerfectSquareMainDirectToDiskMain.model, choice)
    } yield result
  }
}
