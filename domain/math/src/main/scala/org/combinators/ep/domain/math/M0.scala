package org.combinators.ep.domain.math     /*DD:LI:AI*/

import org.combinators.ep.domain._
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.domain.instances._

object M0 extends Evolution {
  //val m0 = Model("m0", Seq(Lit, Add), Seq(Eval))
  override implicit def getModel:Model = MathDomain.getModel.evolve("m0", Seq(Lit, Add), Seq(Eval))

  lazy val litValue = Attribute ("value", TypeRep.Double)

  lazy val Lit = DataTypeCase("Lit", Seq(litValue))
  lazy val Add = DataTypeCase.binary("Add")

  lazy val Eval = Operation("eval", TypeRep.Double)

  //  case class LitInst(d:scala.Double) extends AtomicInst(Lit, ExistsInstance(Double)(d))
  def LitInst(d:scala.Double): DataTypeInstance =
    DataTypeInstance(Lit, Seq(InstanceRep(TypeRep.Double)(d)))

  def AddInst(left:DataTypeInstance, right:DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Add, Seq(InstanceRep(left), InstanceRep(right)))

  // Testing
  def M0_tests:Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, AddInst(LitInst(1.0), LitInst(2.0)), Eval, InstanceRep(LitInst(3.0))),
    EqualsTestCase(getModel.baseDataType, LitInst(5.0), Eval, InstanceRep(LitInst(5.0))),

    PerformanceTestCase(
      11,    // how many iterations to continue the iter
      8,     // how many times to try to find the best
      Eval,
      getModel.baseDataType,
      AddInst(LitInst(1.0), LitInst(2.0)),     // new BinaryInst(Add, LitInst(1.0), LitInst(2.0)),   // base instance
      Seq.empty,   // base parameters
      params => params,   // how parameters evolve (i.e., stay same)
      inst => AddInst( inst, inst)     // object changes with each iteration
    )
  )
}
