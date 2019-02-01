package ep.j  /*DD:LD:AI*/

import com.github.javaparser.ast.body.MethodDeclaration
import ep.domain._
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  * Determine if structure of two Exps are equal to each other. Checking in.
  *
  * First operation that has parameter which has eExp-recursive structure
  */
trait e6 extends Evolution with JavaGenerator with JUnitTestGenerator with OperationDependency with M0 with M5 with M6 {
  self: e0 with e1 with e2 with e3 with e4 with e5 =>
  val domain:MathDomain with ModelDomain

  /**
    * Operations can declare dependencies, which leads to #include extras
    */
  override def dependency(op: domain.Operation): scala.List[domain.Operation] = {
    op match {
      case Equals => scala.List[domain.Operation](domain.AsTree)
      case _ => super.dependency(op)
    }
  }

  abstract override def typeConverter(tpe:domain.TypeRep): com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case Boolean => Java("Boolean").tpe()
      case _ => super.typeConverter(tpe)
    }
  }

  abstract override def logic(exp:domain.Atomic, op:domain.Operation): Seq[Statement] = {
    val source = Source(exp, op)
    op match {
      case Equals =>

        // GOAL: requesting AsTree on self produces same tree as invoking AsTree on that.
        val leftDelta = deltaSelfOp(source, domain.AsTree)
        val that = Java(domain.base.that.name).expression[Expression]()
        val rightDelta = deltaExprOp(source, that, domain.AsTree)
        val lhs:Expression = contextDispatch(source, leftDelta)
        val rhs:Expression = contextDispatch(source, rightDelta)
        result(Java(s"$lhs.same($rhs)").expression())

      case _ => super.logic(exp, op)
    }
  }

  override def junitTestMethod(test:domain.TestCase, idx:Int) : Seq[Statement] = {
      test match {
        case eb: EqualsBinaryMethodTestCase =>
          if (eb.result) {
            Java(s"assertTrue (${dispatch(convert(eb.inst1), Equals, convert(eb.inst2))});").statements
          } else {
            Java(s"assertFalse(${dispatch(convert(eb.inst1), Equals, convert(eb.inst2))});").statements
          }
        case _ => super.junitTestMethod(test, idx)
    }
  }

  /**
    * Construct large trees and determine cost of evaluating over them.
    * @return
    */
  abstract override def performanceMethod(): Seq[UnitTest] = {
    val a1 = new domain.BinaryInst(Add, new LitInst(1.0), new LitInst(2.0))
    val numTrials = 11

    var trees = new domain.BinaryInst(Add, a1, a1)
    var instantiations:String = s"${exprDefine(a1)} tree0  = ${convert(a1)};\n"
    var array:String = s"${exprDefine(a1)} trees[] = { tree0, "
    for (i <- 1 to numTrials) {
      instantiations = instantiations + s"${exprDefine(a1)} tree$i = ${convertRecursive(Add, s"tree${i-1}", s"tree${i-1}")};"
      trees = new domain.BinaryInst(Add, trees, trees)
      array = array + s"tree$i,"
    }
    array = array + "};"

    val source = NoSource()
    val treei = new Java("trees[i]").expression[Expression]()
    val delta = deltaExprOp(source, treei, Equals, treei)
    val toTime = contextDispatch(source, delta)
    val evalPerfTest = Java(
      s"""
         |public void testPerformance() {
         |   System.out.println ("Equals Performance");
         |   $instantiations
         |   $array
         |   for (int i = trees.length-1; i >= 0; i--) {
         |     long best = Long.MAX_VALUE;
         |      for (int t = 0; t < 8; t++) {
         |        long now = System.nanoTime();
         |     		  $toTime;
         |         long duration = System.nanoTime() - now;
         |         if (duration < best) { best = duration; }
         |      }
         |      System.out.println(i + "," + best);
         |   }
         |}""".stripMargin).methodDeclarations.head

    super.performanceMethod() :+ evalPerfTest
  }

  abstract override def testGenerator: Seq[MethodDeclaration] = {
    super.testGenerator ++ testMethod(M6_tests)
  }
}
