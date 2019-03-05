package org.combinators.ep.language.scala    /*DD:LD:AI*/

import org.combinators.ep.domain.math.{M0, M5, MathDomain}
import org.combinators.ep.domain.{Evolution, OperationDependency}

import scala.meta.{Stat, Type}

/**
  * BinaryMethod capability
  *
  * Still Java-based, naturally and JUnit
  */
trait e5 extends Evolution with ScalaGenerator with TestGenerator with OperationDependency with M0 with  M5 {
  self: e0 with e1 with e2 with e3 with e4 =>
  val domain:MathDomain

  /**
    * Operations can declare dependencies, which leads to #include extras
    */
  override def dependency(op: domain.Operation): scala.List[domain.Operation] = {
    op match {
      case domain.AsTree => scala.List[domain.Operation](Identifier)
      case _ => super.dependency(op)
    }
  }

  abstract override def typeConverter(tpe:domain.TypeRep) : Type = {
    tpe match {
      case domain.Tree => Type.Name("tree.Tree")      // package class goes here.
      case _ => super.typeConverter(tpe)
    }
  }

  abstract override def logic(exp:domain.DataType, op:domain.Operation): Seq[Statement] = {
    val source = Source(exp,op)
    op match {
      // Simplify only works for solutions that instantiate expression instances. As a binary
      case domain.AsTree => {
        val atts = subExpressions(exp)

        exp match {
          case Lit =>
            val attParams = atts.map(att => att._2.toString).mkString(",")
            val deltaSelf = deltaSelfOp(Identifier)
            val rhs = contextDispatch(source, deltaSelf)
            result(Scala(s"""new tree.Node(Seq(new tree.Leaf($attParams)), $rhs) """).expression)

          case Add|Sub|Mult|Divd|Neg =>
            val seq = atts.map(att => dispatch(att._2, domain.AsTree)).mkString(",")
            val deltaSelf = deltaSelfOp(Identifier)
            val rhs = contextDispatch(source, deltaSelf)
            result(Scala(s"""new tree.Node(Seq($seq), $rhs ) """).expression)
          }
      }
      case _ => super.logic(exp, op)
    }
  }

  override def scalaTestMethod(test:domain.TestCase, idx:Int) : Seq[Statement] = {
    test match {
      case ctc: SameTestCase =>

        actual(domain.AsTree, ctc.inst1).appendDependent { case Seq(treeLeft) =>
          actual(domain.AsTree, ctc.inst2).appendDependent { case Seq(treeRight) =>
            val same = Scala(s"$treeLeft.same($treeRight)").expression
            CodeBlockWithResultingExpressions(
              if (ctc.result) {
                Scala(s"assert(true == $same)").statement
              } else {
                Scala(s"assert(false == $same)").statement
              }
            )()
          }
        }.block
      case _ => super.scalaTestMethod(test, idx)
    }
  }

  abstract override def testGenerator: Seq[Seq[Stat]] = {
    super.testGenerator ++ testMethod(M5_tests)
  }
}
