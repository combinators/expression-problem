package ep.scala    /*DD:LD:AI*/

import ep.domain._
import org.combinators.ep.domain.math.{M0, M5, M6, MathDomain}
import org.combinators.ep.domain.{Evolution, ModelDomain}

import scala.meta._

/**
  * Truly independent of the specific design solution.
  *
  * Determine if structure of two Exps are equal to each other. Checking in.
  *
  * First operation that has parameter which has eExp-recursive structure
  */
trait e6 extends Evolution with ScalaGenerator with TestGenerator with M0 with M5 with M6 {
  self: e0 with e1 with e2 with e3 with e4 with e5 =>
  val domain:MathDomain with ModelDomain

  abstract override def typeConverter(tpe:domain.TypeRep): Type = {
    tpe match {
      case Boolean => Type.Name("Boolean")
      case _ => super.typeConverter(tpe)
    }
  }

  abstract override def logic(exp:domain.Atomic, op:domain.Operation): Seq[Statement] = {
    val source = Source(exp, op)
    // generate the actual body; since this is a binary method
    op match {
      case Equals =>
        val opn = domain.AsTree.name
        val atts= exp.attributes.map(att => Scala(att.instance).expression)

        val leftDelta = deltaSelfOp(source, domain.AsTree)
        val rightDelta = deltaExprOp(source, Scala("that").expression, domain.AsTree)
        val lhs:Expression = contextDispatch(source, leftDelta)
        val rhs:Expression = contextDispatch(source, rightDelta)
        result(Scala(s"$lhs.same($rhs)").expression)

      case _ => super.logic(exp, op)
    }
  }

  override def scalaTestMethod(test:domain.TestCase, idx:Int) : Seq[Stat] = { // EXTRACT all SameTestCase ones and handle here
     test match {
      case eb: EqualsBinaryMethodTestCase =>
        val code = dispatch(convert(eb.inst1), Equals, convert(eb.inst2))
        if (eb.result) {
          Seq(Scala(s"assert (true == $code)").statement)
        } else {
          Seq(Scala(s"assert (false == $code)").statement)
        }
      case _ => super.scalaTestMethod(test, idx)
    }
  }

  abstract override def testGenerator: Seq[Seq[Stat]] = {
    super.testGenerator ++ testMethod(M6_tests)
  }
}
