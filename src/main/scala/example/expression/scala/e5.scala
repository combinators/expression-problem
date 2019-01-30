package example.expression.scala   /*DD:LD:AI*/

import example.expression.domain._

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

  abstract override def logic(exp:domain.Atomic, op:domain.Operation): Seq[Statement] = {
    val source = Source(exp,op)
    op match {
      // Simplify only works for solutions that instantiate expression instances. As a binary
      case domain.AsTree => {
        val atts = subExpressions(exp)

        exp match {
          case Lit =>
            val attParams = atts.map(att => att._2.toString).mkString(",")
            val deltaSelf = deltaSelfOp(source, Identifier)
            val rhs = contextDispatch(source, deltaSelf)
            result(Scala(s"""new tree.Node(Seq(new tree.Leaf($attParams)), $rhs) """).expression)

          case Add|Sub|Mult|Divd|Neg =>
            val seq = atts.map(att => dispatch(att._2, domain.AsTree)).mkString(",")
            val deltaSelf = deltaSelfOp(source, Identifier)
            val rhs = contextDispatch(source, deltaSelf)
            result(Scala(s"""new tree.Node(Seq($seq), $rhs ) """).expression)
          }
      }
      case _ => super.logic(exp, op)
    }
  }

  override def scalaTestMethod(test:domain.TestCase, idx:Int) : Seq[Stat] = { // EXTRACT all SameTestCase ones and handle here
    test match {
      case ctc: SameTestCase =>
        val source = NoSource()
        val tree1 = contextDispatch(source, deltaExprOp(source, convert(ctc.inst1), domain.AsTree))
        val tree2 = contextDispatch(source, deltaExprOp(source, convert(ctc.inst2), domain.AsTree))

        val same = Scala(s"$tree1.same($tree2)").expression

        if (ctc.result) {
          Scala(s"assert(true == $same)").statements
        } else {
          Scala(s"assert(false == $same)").statements
        }
      case _ => super.scalaTestMethod(test, idx)
    }
  }

  abstract override def testGenerator: Seq[Seq[Stat]] = {
    super.testGenerator ++ testMethod(M5_tests)
  }
}
