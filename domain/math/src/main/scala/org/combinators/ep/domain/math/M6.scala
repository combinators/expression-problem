package org.combinators.ep.domain.math      /*DD:LI:AI*/

import org.combinators.cogen.InstanceRep
import org.combinators.ep.domain.instances.DataTypeInstanceRep
import org.combinators.cogen.{Tag, TestCase, TypeRep}
import org.combinators.ep.domain.{Evolution, GenericModel}
import org.combinators.ep.domain.abstractions.{DataTypeCase, DomainTpeRep, EqualsTestCase, Operation, Parameter, PerformanceTestCase}
import org.combinators.ep.domain.instances.DataTypeInstance
import org.combinators.ep.domain.math.M0.{Add, AddInst, LitInst}
import org.combinators.ep.domain.math.M1.SubInst
import org.combinators.ep.domain.math.M3.{DivdInst, MultInst, NegInst}

object M6 extends Evolution {
  override implicit def getModel:GenericModel = M5.getModel.evolve("m6", Seq.empty, Seq(Equals,Eql) ++ isOps(allTypes))

  lazy val allTypes = M5.getModel.flatten.typeCases
  def isOps(tpeCases:Seq[DataTypeCase]) : Seq[Operation] = {
    tpeCases.map(tpe => isOp(tpe))
  }

  case object IsOp extends Tag

  def isOp(tpeCase:DataTypeCase): Operation = {
    val args = tpeCase.attributes.map(att => Parameter(att.name, att.tpe))
    Operation("is" + tpeCase.name.capitalize, TypeRep.Boolean, args, Seq(IsOp))
  }

  // Binary Methods, by definition, require special handling. Some generators
  // can short-circuit this logic, but in the most general case, it stands to reason
  // that we need to have a way to instantiate a structure that matches the expression
  // and then use those structure(s) to determine equality.
  // TODO: HACK -- have to manually change 'equals' to something other than equals
  lazy val Equals = Operation("ep_equals", TypeRep.Boolean, Seq(Parameter("other", DomainTpeRep.DataType(M5.getModel.baseDataType))))

  // EQL depends on past IsXXX which you know from ALL PAST evolutions
  lazy val Eql = Operation("eql", TypeRep.Boolean, Seq(Parameter("other", DomainTpeRep.DataType(M5.getModel.baseDataType))))

  val m6_s1 = SubInst(LitInst(1.0), LitInst(73.0))
  val m6_s2 = SubInst(LitInst(1.0), LitInst(73.0))
  val m6_s3 = AddInst(LitInst(5.0), LitInst(3.0))

  val m6_m1 = MultInst(DivdInst(LitInst(5.0),  LitInst(2.0)), LitInst(4.0))
  val m6_m2 = MultInst(DivdInst(LitInst(5.0),  LitInst(2.0)), LitInst(3.0))
  val m6_m3 = NegInst(m6_m1)

  val m6_d3 = DivdInst(LitInst(6.0), LitInst(2.0))
  val m6_d4 = DivdInst(LitInst(8.0), LitInst(2.0))

  object EqualsBinaryMethodTestCase {
    def apply(op:Operation,instance: DataTypeInstance, instance1: DataTypeInstance, result: Boolean): TestCase = {
      EqualsTestCase(
        M5.getModel.baseDataType,
        instance,
        op,
        InstanceRep(TypeRep.Boolean)(result),
        DataTypeInstanceRep.apply(instance1)(getModel)
      )
    }
  }

  override def allTests: Map[GenericModel, Seq[TestCase]] = allPastTests(M5)

  def tests:Seq[TestCase] = Seq(
    EqualsBinaryMethodTestCase(Equals, m6_s2, m6_s1, result=true),  // parameter to operation
    EqualsBinaryMethodTestCase(Equals, m6_m1, m6_m2, result=false),  // parameter to operation
    EqualsBinaryMethodTestCase(Equals, m6_m1, m6_m1, result=true),  // parameter to operation

    EqualsBinaryMethodTestCase(Equals, m6_m3, m6_m3, result=true),  // parameter to operation
    EqualsBinaryMethodTestCase(Equals, m6_m1, m6_m3, result=false),  // parameter to operation
    EqualsBinaryMethodTestCase(Equals, m6_d3, m6_d4, result=false),  // parameter to operation
    EqualsBinaryMethodTestCase(Equals, m6_d3, m6_d3, result=true),  // parameter to operation
    EqualsBinaryMethodTestCase(Equals, m6_s3, m6_s3, result=true),  // parameter to operation
    EqualsBinaryMethodTestCase(Equals, m6_s3, m6_m2, result=false),  // parameter to operation

    EqualsBinaryMethodTestCase(Eql, m6_s2, m6_s1, result=true),  // parameter to operation
    EqualsBinaryMethodTestCase(Eql, m6_m1, m6_m2, result=false),  // parameter to operation
    EqualsBinaryMethodTestCase(Eql, m6_m1, m6_m1, result=true),  // parameter to operation

    EqualsBinaryMethodTestCase(Eql, m6_m3, m6_m3, result=true),  // parameter to operation
    EqualsBinaryMethodTestCase(Eql, m6_m1, m6_m3, result=false),  // parameter to operation
    EqualsBinaryMethodTestCase(Eql, m6_d3, m6_d4, result=false),  // parameter to operation
    EqualsBinaryMethodTestCase(Eql, m6_d3, m6_d3, result=true),  // parameter to operation
    EqualsBinaryMethodTestCase(Eql, m6_s3, m6_s3, result=true),  // parameter to operation
    EqualsBinaryMethodTestCase(Eql, m6_s3, m6_m2, result=false),  // parameter to operation

    PerformanceTestCase(
      11,
      8,
      Equals,
      getModel.baseDataType,
      AddInst(LitInst(1.0), LitInst(2.0)),     // first, base instance

      // initial parameter to use when testing equals
      Seq(DataTypeInstanceRep(AddInst(LitInst(1.0), LitInst(2.0)))),

      // function tells how InstanceRep parameters evolve with each iteration
      // Seq[InstanceRep] => Seq[InstanceRep]
      params => params.map(param =>
        param.inst match {
          case i: InstanceRep => DataTypeInstanceRep(DataTypeInstance(Add, Seq(i,i)))
          case _ => param
        }),

      inst => AddInst(inst, inst)   // function tells how objects evolve with each iteration
    )
  )
}
