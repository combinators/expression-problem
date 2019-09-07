package org.combinators.ep.domain.math      /*DD:LI:AI*/

import org.combinators.ep.domain.{Evolution, Model}
import org.combinators.ep.domain.abstractions.{Operation, PerformanceTestCase, TestCase, TypeRep}
import org.combinators.ep.domain.instances.{DataTypeInstance, InstanceRep}
import org.combinators.ep.domain.math.M0.{Add, AddInst, LitInst}
import org.combinators.ep.domain.math.M1.{Sub, SubInst}
import org.combinators.ep.domain.math.M3.{Divd, DivdInst, Mult, MultInst, Neg, NegInst}



object M6 extends Evolution {
  override implicit def getModel:Model = M5.getModel.evolve("m6", Seq.empty, Seq(Equals))

  // Binary Methods, by definition, require special handling. Some generators
  // can short-circuit this logic, but in the most general case, it stands to reason
  // that we need to have a way to instantiate a structure that matches the expression
  // and then use those structure(s) to determine equality.

  lazy val Equals = Operation("equals", TypeRep.Boolean)

  /**
    * Special test case for binary methods.
    *
    * Validates that calling AsTree on inst1 yields the tree called from AsTree on inst2
    */
  case class EqualsBinaryMethodTestCase(
      inst1:DataTypeInstance,
      inst2:DataTypeInstance,
      result:Boolean) extends TestCase

  val m6_s1 = SubInst(LitInst(1.0), LitInst(73.0))
  val m6_s2 = SubInst(LitInst(1.0), LitInst(73.0))
  val m6_s3 = AddInst(LitInst(5.0), LitInst(3.0))

  val m6_m1 = MultInst(DivdInst(LitInst(5.0),  LitInst(2.0)), LitInst(4.0))
  val m6_m2 = MultInst(DivdInst(LitInst(5.0),  LitInst(2.0)), LitInst(3.0))
  val m6_m3 = NegInst(m6_m1)

  val m6_d3 = DivdInst(LitInst(6.0), LitInst(2.0))
  val m6_d4 = DivdInst(LitInst(8.0), LitInst(2.0))

  def M6_tests:Seq[TestCase] = Seq(
    EqualsBinaryMethodTestCase(m6_s2, m6_s1, result=true),  // parameter to operation
    EqualsBinaryMethodTestCase(m6_m1, m6_m2, result=false),  // parameter to operation
    EqualsBinaryMethodTestCase(m6_m1, m6_m1, result=true),  // parameter to operation

    EqualsBinaryMethodTestCase(m6_m3, m6_m3, result=true),  // parameter to operation
    EqualsBinaryMethodTestCase(m6_m1, m6_m3, result=false),  // parameter to operation
    EqualsBinaryMethodTestCase(m6_d3, m6_d4, result=false),  // parameter to operation
    EqualsBinaryMethodTestCase(m6_d3, m6_d3, result=true),  // parameter to operation
    EqualsBinaryMethodTestCase(m6_s3, m6_s3, result=true),  // parameter to operation
    EqualsBinaryMethodTestCase(m6_s3, m6_m2, result=false),  // parameter to operation

    PerformanceTestCase(
      11,
      8,
      Equals,
      AddInst(LitInst(1.0), LitInst(2.0)),     // first, base instance

      // initial parameter to use when testing equals
      Seq(InstanceRep(AddInst(LitInst(1.0), LitInst(2.0)))),

      // function tells how InstanceRep parameters evolve with each iteration
      // Seq[InstanceRep] => Seq[InstanceRep]
      params => params.map(param =>
        param.inst match {
          case i: InstanceRep => InstanceRep(DataTypeInstance(Add, Seq(i,i)))
          case _ => param
        }),

      inst => AddInst(inst, inst)   // function tells how objects evolve with each iteration
    )
  )
}
