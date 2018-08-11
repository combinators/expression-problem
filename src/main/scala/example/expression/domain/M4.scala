package example.expression.domain     /*DD:LI:AI*/

trait M4 extends Evolution  {
  self: M0 with M1 with M2 with M3 =>
  val domain:MathDomain

  // m4:model evolution.
  // -------------------
  case object Simplify extends domain.ProducerOperation("simplify")
  case class List(generic:domain.TypeRep) extends domain.TypeRep
  case object Collect extends domain.Operation("collect", Some(List(Double)))

  val m4 = domain.Model("m4",Seq.empty, Seq(Simplify, Collect), last = m3)
  override def getModel = m4

//  /**
//    * Simplify depends on Eval (during simplification process) and PrettyP (during testing).
//    */
//  override def dependency(op: domain.Operation): scala.List[domain.Operation] = {
//    if (op.equals(Simplify)) {
//      super.dependency(op) ++ scala.List[domain.Operation](PrettyP,Eval)
//    } else {
//      super.dependency(op)
//    }
//  }
}
