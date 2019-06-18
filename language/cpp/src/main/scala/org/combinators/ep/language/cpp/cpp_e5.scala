package org.combinators.ep.language.cpp    /*DD:LD:AI*/

import org.combinators.ep.domain.Evolution
import org.combinators.ep.domain.math._

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
      case domain.TreeType => new CPPType(s"Tree *")      // internal interface (make pointer)
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
  abstract override def logic(exp:DataType, op:Operation): Seq[CPPStatement] = {
    val source = Source(exp,op)

    op match {
      case domain.AsTree =>
        val atts = subExpressions(exp)

        exp match {
          case Lit =>
            val attParams = atts.map(att => valueOf(atts(att._2.toString))).mkString(",")
            result(new CPPExpression(s""" new Leaf( $attParams) """))

          case Add|Sub|Mult|Divd|Neg =>
            val attParams = atts.map(att => new CPPExpression(s"${valueOf(atts(att._2.toString))}->astree()")).mkString(",")
            val vec1 = new CPPStatement(s"std::vector<Tree *> vec_${exp.name} = { $attParams };")

            val deltaSelf = dispatchSelf(Identifier)
            val rhs = contextDispatch(source, deltaSelf)
            Seq(vec1) ++ result(new CPPExpression(s" new Node(vec_${exp.name}, $rhs) "))
        }

        // moved here from cpp_e0
      case Identifier => result(new CPPExpression(exp.hashCode().toString))

      case _ => super.logic(exp, op)
    }
  }

  override def cppUnitTestMethod(test:TestCase, idx:Int) : Seq[Statement] = {
    test match {
      case ctc: SameTestCase =>
        actual(AsTree, ctc.inst1).appendDependent { case Seq(treeLeft) =>
          actual(AsTree, ctc.inst2).appendDependent { case Seq(treeRight) =>
            val same = new CPPExpression(s"$treeLeft->same($treeRight)")
            CodeBlockWithResultingExpressions(
              if (ctc.result) {
                new CPPStatement(s"CHECK_TRUE($same);")
              } else {
                new CPPStatement(s"CHECK_FALSE($same);")
              }
            )()
          }
        }.block

      case _ => super.cppUnitTestMethod(test, idx)
    }
  }

  abstract override def testGenerator: Seq[Seq[CPPElement]] = {
    super.testGenerator ++ testMethod(M5_tests)
  }
}
