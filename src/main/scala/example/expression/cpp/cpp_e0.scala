package example.expression.cpp     /*DD:LD:AI*/

import example.expression.domain.M0

/**
  * Truly independent of the specific design solution.
  *
  * Still C++-based, naturally and CPPUnit
  */
trait cpp_e0 extends CPPGenerator with TestGenerator with M0 {
  import domain._

  /** E0 Introduces the concept a Double type, used for the 'Eval' operation. */
  abstract override def typeConverter(tpe:TypeRep) : CPPType = {
    tpe match {
      case Double => new CPPType("double")
      case Int => new CPPType("int")
      case _ => super.typeConverter(tpe)
    }
  }

  /** Eval operation needs to provide specification for current datatypes, namely Lit and Add. */
  abstract override def logic(exp:Atomic, op:Operation): Seq[CPPElement] = {
    val atts = subExpressions(exp)

    // generate the actual body
    op match {
      case Eval =>
        exp match {
          case Lit => result(valueOf(atts(litValue)))
          case Add => result(new CPPElement(s"${dispatch(atts(base.left), op)} + ${dispatch(atts(base.right), op)}"))

          case _ => super.logic(exp, op)
        }

      // all future EXP sub-types can simply return hashcode.
      case Identifier => result(new CPPElement(exp.hashCode().toString))

      case _ => super.logic(exp, op)
    }
  }

  /**
    * Construct large trees and determine cost of evaluating over them.
    * @return
    */
  abstract override def performanceMethod: Seq[CPPElement] = {
    val a1 = new BinaryInst(Add, new LitInst(1.0), new LitInst(2.0))
    val numTrials = 10

    var trees = new BinaryInst(Add, a1, a1)
    var instantiations:String = s"Exp *tree0 = ${rec_convert(a1)};\n"
    var array:String = s"Exp *trees[] = {tree0 "
    for (i <- 1 to numTrials) {
      instantiations = instantiations + s"Exp *tree$i = ${convertRecursive(Add, s"tree${i-1}", s"tree${i-1}")};\n"
      trees = new BinaryInst(Add, trees, trees)
      array = array + s",tree$i"
    }
    array = array + "};"

    val source = NoSource()
    val delta = deltaExprOp(source, new CPPElement("trees[i]"), Eval)   // was independentExpr
    val toTime = contextDispatch(source, delta)
    val evalPerfTest:CPPElement = new CPPElement(
      s"""
         |$instantiations
         |$array
         |
         |for (int i = 10; i >= 0; i--) {
         |  long best = 2147483647;
         |  struct timeval before;     /** Time before process starts.   */
         |  struct timeval after;      /** Time after process completes. */
         |
         |  for (int t = 0; t < 8; t++) {
         |    gettimeofday(&before, (struct timezone *) NULL);
         |    $toTime;
         |    gettimeofday(&after, (struct timezone *) NULL);
         |    long duration = diffTimer(&before, &after);
         |    if (duration < best) {
         |      best = duration;
         |    }
         |  }
         |  std::cout << i << "," << best << std::endl;
         |}""".stripMargin)

    super.performanceMethod :+ evalPerfTest
  }

  abstract override def testGenerator: Seq[CPPElement] = {
    val tests = new CPPElement(testMethod(M0_tests).mkString("\n"))

    super.testGenerator :+ tests
  }
}
