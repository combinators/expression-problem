package ep.cpp    /*DD:LD:AI*/

import ep.domain.{Evolution, M0, M1, M2, M3, M4, M5}

/**
  * Truly independent of the specific design solution.
  *
  * Still C++-based, naturally and CPPUnit
  */
trait cpp_e5 extends Evolution with CPPGenerator with TestGenerator with M0 with M1 with M2 with M3 with M4 with M5 {
  self:cpp_e0 with cpp_e1 with cpp_e2 with cpp_e3 with cpp_e4 =>

  import domain._

  abstract override def typeConverter(tpe:domain.TypeRep) : CPPType = {
    tpe match {
      case domain.Tree => new CPPType(s"Tree *")      // internal interface (make pointer)
      case _ => super.typeConverter(tpe)
    }
  }

  /**
    * Operations can declare dependencies, which leads to #include extras
    */
  override def dependency(op: domain.Operation): scala.List[domain.Operation] = {
    op match {
      case AsTree => scala.List[domain.Operation](Identifier)
      case _ => super.dependency(op)
    }
  }

  /** Eval operation needs to provide specification for current datatypes, namely Lit and Add. */
  abstract override def logic(exp:Atomic, op:Operation): Seq[CPPElement] = {
    val source = Source(exp,op)

    op match {
      case domain.AsTree =>
        val atts = subExpressions(exp)

        exp match {
          case Lit =>
            val attParams = atts.map(att => valueOf(atts(att._2.toString))).mkString(",")
            result(new CPPElement(s""" new Leaf( $attParams) """))

          case Add|Sub|Mult|Divd|Neg =>
            val attParams = atts.map(att => new CPPElement(s"${valueOf(atts(att._2.toString))}->astree()")).mkString(",")
            val vec1 = new CPPElement(s"std::vector<Tree *> vec_${exp.name} = { $attParams };")

            val deltaSelf = deltaSelfOp(source, Identifier)
            val rhs = contextDispatch(source, deltaSelf)
            Seq(vec1) ++ result(new CPPElement(s" new Node(vec_${exp.name}, $rhs) "))
        }

      case _ => super.logic(exp, op)
    }
  }

  //  abstract override def testMethod(tests:Seq[domain.TestCase]) : Seq[CPPElement] = {
  override def cppUnitTestMethod(test:TestCase, idx:Int) : Seq[Statement] = {
      test match {
        case ctc: SameTestCase =>
          val tree1 = actual(AsTree, ctc.inst1)
          val tree2 = actual(AsTree, ctc.inst2)

          if (ctc.result) {
            Seq(new CPPElement(s"CHECK_TRUE($tree1->same($tree2));"))
          } else {
            Seq(new CPPElement(s"CHECK_TRUE(!$tree1->same($tree2));"))
          }

        case _ =>
          super.cppUnitTestMethod(test, idx)
      }
    }

  abstract override def testGenerator: Seq[Seq[CPPElement]] = {
    super.testGenerator ++ testMethod(M5_tests)
  }
}