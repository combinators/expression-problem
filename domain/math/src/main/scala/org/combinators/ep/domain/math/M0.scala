package org.combinators.ep.domain.math     /*DD:LI:AI*/

import org.combinators.ep.domain._
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.domain.instances._

object M0 extends Evolution {
  override implicit def getModel:GenericModel = MathDomain.baseModel.evolve("m0", Seq(Lit, Add), Seq(Eval))
  lazy val litValue = Attribute ("value", TypeRep.Double)

  lazy val Lit = DataTypeCase("Lit", Seq(litValue))
  lazy val Add = DataTypeCase.binary("Add")(MathDomain.getModel)

  lazy val Eval = Operation("eval", TypeRep.Double)

  def DoubleInst(d: scala.Double): InstanceRep =
    InstanceRep(TypeRep.Double)(d)

  //  case class LitInst(d:scala.Double) extends AtomicInst(Lit, ExistsInstance(Double)(d))
  def LitInst(d:scala.Double): DataTypeInstance =
    DataTypeInstance(Lit, Seq(DoubleInst(d)))

  def AddInst(left:DataTypeInstance, right:DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Add, Seq(InstanceRep(left), InstanceRep(right)))

  val addi:DataTypeInstance = AddInst(LitInst(1.0), LitInst(2.0))
  val liti:DataTypeInstance = LitInst(5.0)

  // defining tests exactly once means they can be eliminated later (in Evolution) when it uses 'distinct' on past test cases)
  val m0_test_1: TestCase = EqualsTestCase(getModel.baseDataType, addi, Eval, DoubleInst(3.0))
  val m0_test_2: TestCase = EqualsTestCase(getModel.baseDataType, liti, Eval, DoubleInst(5.0))

  // Testing
  def tests: Seq[TestCase] = Seq(
    m0_test_1,
    m0_test_2,

    PerformanceTestCase(
      11,    // how many iterations to continue the iteration
      8,     // how many times to try to find the best
      Eval,
      getModel.baseDataType,
      addi,          // base instance
      Seq.empty,     // base parameters
      params => params,   // how parameters evolve (i.e., stay same)
      inst => AddInst( inst, inst)     // object changes with each iteration
    )
  )
}
