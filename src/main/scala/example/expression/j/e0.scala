package example.expression.j   /*DD:LD:AI*/

import com.github.javaparser.ast.body.MethodDeclaration
import example.expression.domain.M0
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e0 extends JavaGenerator with JUnitTestGenerator with M0 {
  import domain._

  /** E0 Introduces the concept a Double type, used for the 'Eval' operation. */
  abstract override def typeConverter(tr:TypeRep) : Type = {
    tr match {
      case Double => Java("Double").tpe
      case Int => Java("Integer").tpe
      case _ => super.typeConverter(tr)
    }
  }

  /** Eval operation needs to provide specification for current datatypes, namely Lit and Add. */
  abstract override def logic(exp:Atomic, op:Operation): Seq[Statement] = {
    val atts = subExpressions(exp)

    // generate the actual body
    op match {
      case Eval =>
        exp match {
          case Lit => result(Java(atts(litValue)).expression[Expression]())
          case Add => result(Java(s"${dispatch(atts(base.left),op)} + ${dispatch(atts(base.right),op)}").expression[Expression]())
          case _ => super.logic(exp, op)
        }

        // all future EXP sub-types can simply return hashcode.
      case Identifier => result(Java(exp.hashCode.toString).expression[Expression]())

      case _ => super.logic(exp, op)
    }
  }

  /**
    * Construct large trees and determine cost of evaluating over them.
    * @return
    */
  abstract override def performanceMethod(): Seq[UnitTest] = {
    val a1 = new BinaryInst(Add, new LitInst(1.0), new LitInst(2.0))
    val numTrials = 10

    var trees = new BinaryInst(Add, a1, a1)
    var instantiations:String = s"${exprDefine(a1)} tree0  = ${convert(a1)};\n"
    var array:String = s"${exprDefine(a1)} trees[] = { tree0, "
    for (i <- 1 to numTrials) {
      instantiations = instantiations + s"${exprDefine(a1)} tree$i = ${convertRecursive(Add, s"tree${i-1}", s"tree${i-1}")};"
      trees = new BinaryInst(Add, trees, trees)
      array = array + s"tree$i,"
    }
    array = array + "};"

    val source = NoSource()
    val delta = deltaExprOp(source, new Java("trees[i]").expression[Expression](), Eval)
    val toTime = contextDispatch(source, delta)
    val evalPerfTest = Java(
      s"""
         |public void testPerformance() {
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
    super.testGenerator ++ testMethod(M0_tests)
  }
}
