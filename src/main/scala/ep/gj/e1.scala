package ep.gj    /*DD:LD:AI*/

import ep.domain.{Evolution, M1, MathDomain}

/**
  * Truly independent of the specific design solution.
  */
trait e1 extends Evolution with GJGenerator with TestGenerator with M1 {
  self:e0 =>
  val domain:MathDomain

  abstract override def logic(exp:domain.Atomic, op:domain.Operation): Seq[GJ] = {
    op match {
      case Eval =>
        exp match {
          case Sub => Seq(GJ(s"return new Double(${dispatch(expression(exp,domain.base.left), Eval)}.doubleValue() - ${dispatch(expression(exp,domain.base.right), Eval)}.doubleValue());"))
          case _ => super.logic(exp, op)
        }

      case _ => super.logic(exp, op)
    }
  }

  abstract override def testGenerator: Seq[GJ] = {
    val s1 = new domain.BinaryInst(Sub, new LitInst(1.0), new LitInst(2.0))
    val modName = getModel.name

    super.testGenerator ++ Seq(GJ(
      s"""|   Lang$modName l = new Lang$modName();
          |   assertEquals(-1.0, ${testDispatch(convert(s1), Eval)});
          |""".stripMargin))
  }
}