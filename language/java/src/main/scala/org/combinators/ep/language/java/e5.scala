package org.combinators.ep.language.java

/*DD:LD:AI*/

import com.github.javaparser.ast.body.MethodDeclaration
import org.combinators.ep.domain.math.{M0, M5, MathDomain}
import org.combinators.ep.domain.{Evolution, OperationDependency}
import org.combinators.templating.twirl.Java

/**
  * BinaryMethod capability
  *
  * Still Java-based, naturally and JUnit
  */
trait e5 extends Evolution with JavaGenerator with JUnitTestGenerator with OperationDependency with M0 with M5 {
  self: e0 with e1 with e2 with e3 with e4 =>
  val domain:MathDomain
  import domain._

  /**
    * Operations can declare dependencies, which leads to #include extras
    */
  override def dependency(op: domain.Operation): scala.List[domain.Operation] = {
    op match {
      case domain.AsTree => scala.List[domain.Operation](Identifier)
      case _ => super.dependency(op)
    }
  }

  abstract override def typeConverter(tpe:domain.TypeRep) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case domain.Tree => Java(s"tree.Tree").tpe()      // package class goes here.
      case _ => super.typeConverter(tpe)
    }
  }

  /** helper method for testing when goal is to check equality between tree objects . */
  def treesAreSame(treeExp1:Expression, treeExp2:Expression) : Expression = {
    Java(s"${treeExp1.toString}.same(${treeExp2.toString})").expression[Expression]()
  }

  abstract override def logic(exp:domain.DataType, op:domain.Operation): Seq[Statement] = {
    // generate the actual body
    val source = Source(exp,op)
    op match {
      // Simplify only works for solutions that instantiate expression instances. As a binary
      case domain.AsTree =>
        val atts = subExpressions(exp)

        exp match {
          case Lit =>
            val attParams = atts.map(att => att._2.toString).mkString(",")
            val deltaSelf = deltaSelfOp(source, Identifier)
            val rhs = contextDispatch(source, deltaSelf)
            result(Java(s" new tree.Node(java.util.Arrays.asList(new tree.Leaf($attParams)), $rhs) ").expression[Expression]())

          case Add|Sub|Mult|Divd|Neg =>
            val attParams = atts.map(att => att._2.toString + ".astree()").mkString(",")
            val deltaSelf = deltaSelfOp(source, Identifier)
            val rhs = contextDispatch(source, deltaSelf)
            result(Java(s" new tree.Node(java.util.Arrays.asList($attParams), $rhs) ").expression[Expression]())
          }

      case _ => super.logic(exp, op)
    }
  }

  override def junitTestMethod(test:TestCase, idx:Int) : Seq[Statement] = {
      test match {
        case ctc: SameTestCase =>
            actual(AsTree, ctc.inst1).appendDependent { case Seq(treeLeft) =>
              actual(AsTree, ctc.inst1).appendDependent { case Seq(treeRight) =>
                val same = Java(s"$treeLeft.same($treeRight)").expression[Expression]()
                CodeBlockWithResultingExpressions(
                  if (ctc.result) {
                    Java(s"assertTrue($same);").statement()
                  } else {
                    Java(s"assertFalse($same").statement()
                  }
                )()
              }
            }.block
        case _ => super.junitTestMethod(test, idx)
      }
    }

  abstract override def testGenerator: Seq[MethodDeclaration] = {
    super.testGenerator ++ testMethod(M5_tests)
  }
}
