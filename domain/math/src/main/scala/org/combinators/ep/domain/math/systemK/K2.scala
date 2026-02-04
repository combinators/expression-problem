package org.combinators.ep.domain.math.systemK    /*DD:LI:AI*/

import org.combinators.cogen.InstanceRep
import org.combinators.cogen.TypeRep
import org.combinators.cogen.TestCase
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.domain.instances.DataTypeInstance
import org.combinators.ep.domain.math.M0.{AddInst, Eval, LitInst, addi}
import org.combinators.ep.domain.math.M1.SubInst
import org.combinators.ep.domain.math.M3.MultInst
import org.combinators.ep.domain.math.systemJ.J1.subi
import org.combinators.ep.domain.math.systemJ.J2.multi
import org.combinators.ep.domain.math.systemK.K1.{PowerInst, powi}
import org.combinators.ep.domain.math.{M0, MathDomain}
import org.combinators.ep.domain.{Evolution, GenericModel}

object K2 extends Evolution {
  override implicit def getModel: GenericModel = K1.getModel.evolve("k2", Seq.empty, Seq(Simplify, Collect))

  // this is a producer method (as you can tell by its return type).
  lazy val Simplify: Operation = Operation("simplify", DomainTpeRep.DataType(MathDomain.getModel.baseDataType))

  def ListDoubleInst(doubles: Seq[scala.Double]): InstanceRep = InstanceRep(TypeRep.Sequence(TypeRep.Double))(doubles)

  lazy val Collect: Operation = Operation("collect", TypeRep.Sequence(TypeRep.Double))

  val m4_m2: DataTypeInstance = MultInst(LitInst(2.0), LitInst(3.0))

  val m4_s_5: DataTypeInstance = AddInst(LitInst(5.0), LitInst(0.0))
  val m4_s_00: DataTypeInstance = AddInst(LitInst(0.0), LitInst(0.0))
  val m4_s_7: DataTypeInstance = AddInst(LitInst(0.0), LitInst(7.0))

  // validates simplify ((5+0)+(0+7)) = (5+7)
  val m4_together: DataTypeInstance = AddInst(m4_s_5, m4_s_7)
  val m4_s_13: DataTypeInstance = MultInst(LitInst(13.0), LitInst(1.0))
  val m4_s_12: DataTypeInstance = MultInst(LitInst(1.0), LitInst(12.0))
  val m4_s_m0: DataTypeInstance = SubInst(LitInst(7.0), LitInst(7.0))

  override def allTests: Map[GenericModel, Seq[TestCase]] = allPastTests(K1)

  /**
   * Test cases for Simplify are oddly complicated. The Simplify operation returns a new Exp object, but
   * making test cases depends upon having the ability to PrettyP the result. We therefore want to check
   * equality of (d1 x prettyP) vs. ((d2 x Simplify) x prettyp)
   *
   * Result should support a composite operation
   */
  def tests: Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, m4_s_00, Collect, ListDoubleInst(Seq(0.0, 0.0))),
    EqualsTestCase(getModel.baseDataType, m4_s_12, Collect, ListDoubleInst(Seq(1.0, 12.0))),
    EqualsTestCase(getModel.baseDataType, m4_s_13, Collect, ListDoubleInst(Seq(13.0, 1.0))),
    EqualsTestCase(getModel.baseDataType, powi, Collect, ListDoubleInst(Seq(3.0, 5.0))),

    EqualsCompositeTestCase(getModel.baseDataType, m4_s_5, M0.DoubleInst(5.0), (Simplify, Seq.empty), (Eval, Seq.empty)),
    EqualsCompositeTestCase(getModel.baseDataType, m4_s_7, M0.DoubleInst(7.0), (Simplify, Seq.empty), (Eval, Seq.empty)),
    EqualsCompositeTestCase(getModel.baseDataType, m4_s_13, M0.DoubleInst(13.0), (Simplify, Seq.empty), (Eval, Seq.empty)),
    EqualsCompositeTestCase(getModel.baseDataType, m4_s_12, M0.DoubleInst(12.0), (Simplify, Seq.empty), (Eval, Seq.empty)),
    EqualsCompositeTestCase(getModel.baseDataType, m4_s_m0, M0.DoubleInst(0.0), (Simplify, Seq.empty), (Eval, Seq.empty)),

    EqualsCompositeTestCase(getModel.baseDataType, AddInst(LitInst(0.0), LitInst(5.0)), M0.DoubleInst(5.0), (Simplify, Seq.empty), (Eval, Seq.empty)),
    EqualsCompositeTestCase(getModel.baseDataType, AddInst(LitInst(5.0), LitInst(0.0)), M0.DoubleInst(5.0), (Simplify, Seq.empty), (Eval, Seq.empty)),
    EqualsCompositeTestCase(getModel.baseDataType, addi, M0.DoubleInst(3.0), (Simplify, Seq.empty), (Eval, Seq.empty)),
    EqualsCompositeTestCase(getModel.baseDataType, subi, M0.DoubleInst(-1.0), (Simplify, Seq.empty), (Eval, Seq.empty)),
    EqualsCompositeTestCase(getModel.baseDataType, multi, M0.DoubleInst(6.0), (Simplify, Seq.empty), (Eval, Seq.empty)),
    EqualsCompositeTestCase(getModel.baseDataType, AddInst(LitInst(-5.0), LitInst(5.0)), M0.DoubleInst(0.0), (Simplify, Seq.empty), (Eval, Seq.empty)),

    EqualsCompositeTestCase(getModel.baseDataType, PowerInst(LitInst(5.0), LitInst(1.0)), M0.DoubleInst(5.0), (Simplify, Seq.empty), (Eval, Seq.empty)),
    EqualsCompositeTestCase(getModel.baseDataType, PowerInst(LitInst(1.0), LitInst(9.0)), M0.DoubleInst(1.0), (Simplify, Seq.empty), (Eval, Seq.empty)),
    EqualsCompositeTestCase(getModel.baseDataType, PowerInst(LitInst(5.0), LitInst(0.0)), M0.DoubleInst(1.0), (Simplify, Seq.empty), (Eval, Seq.empty)),
  )
}
