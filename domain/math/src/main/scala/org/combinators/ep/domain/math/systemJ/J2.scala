package org.combinators.ep.domain.math.systemJ    /*DD:LI:AI*/

import org.combinators.ep.domain.abstractions._
import org.combinators.ep.domain.instances.{DataTypeInstance, InstanceRep}
import org.combinators.ep.domain.math.M0.{AddInst, DoubleInst, Eval, LitInst, addi, liti}
import org.combinators.ep.domain.math.systemJ.J1.{MultByTestCase, SubInst, subi}
import org.combinators.ep.domain.math.{M0, MathDomain}
import org.combinators.ep.domain.{Evolution, GenericModel}

object J2 extends Evolution {
  override implicit def getModel: GenericModel = J1.getModel.evolve("j2", Seq(Mult), Seq(Eql) ++ isOps(allTypes))

  lazy val allTypes = J1.getModel.flatten.typeCases :+ Mult

  def isOps(tpeCases: Seq[DataTypeCase]): Seq[Operation] = {
    tpeCases.map(tpe => isOp(tpe))
  }

  case object IsOp extends Tag

  def isOp(tpeCase: DataTypeCase): Operation = {
    val args = tpeCase.attributes.map(att => Parameter(att.name, att.tpe))
    Operation("is" + tpeCase.name.capitalize, TypeRep.Boolean, args, Seq(IsOp))
  }

  def BooleanInst(b: scala.Boolean): InstanceRep =
    InstanceRep(TypeRep.Boolean)(b)

  // Binary Methods, by definition, require special handling. Some generators
  // can short-circuit this logic, but in the most general case, it stands to reason
  // that we need to have a way to instantiate a structure that matches the expression
  // and then use those structure(s) to determine equality.

  // EQL depends on past IsXXX which you know from ALL PAST evolutions
  lazy val Eql = Operation("eql", TypeRep.Boolean, Seq(Parameter("other", TypeRep.DataType(J1.getModel.baseDataType)))) // base case from prior evolution

  lazy val Mult = DataTypeCase.binary("Mult")(MathDomain.getModel)

  def MultInst(left: DataTypeInstance, right: DataTypeInstance): DataTypeInstance =
    DataTypeInstance(Mult, Seq(InstanceRep(left), InstanceRep(right)))

  // Tests
  // (5/7) / (7-(2*3) --> just (5/7)
  val multi = MultInst(LitInst(2.0), LitInst(3.0))

  object EqualsBinaryMethodTestCase {
    def apply(op: Operation, instance: DataTypeInstance, instance1: DataTypeInstance, result: Boolean): TestCase = {
      EqualsTestCase(
        J2.getModel.baseDataType,
        instance,
        op,
        InstanceRep(TypeRep.Boolean)(result),
        InstanceRep.apply(instance1)(getModel)
      )
    }
  }

  val addi_same_lhs = AddInst(LitInst(1.0), LitInst(3.0))
  val addi_same_rhs = AddInst(LitInst(3.0), LitInst(2.0))
  val subi_same_lhs = SubInst(LitInst(1.0), LitInst(3.0))
  val subi_same_rhs = SubInst(LitInst(3.0), LitInst(2.0))
  val multi_same_lhs = MultInst(LitInst(1.0), LitInst(3.0))
  val multi_same_rhs = MultInst(LitInst(3.0), LitInst(2.0))

  // generate all mismatched Eqls from this
  // also cover all cases by creating pairs of Op(val1,val2) to be compared against Op(val1,val3) and Op(val3,val2)
  val all_instances = Seq(liti, addi, subi, multi)
  val lhs = Seq(LitInst(-99), addi_same_lhs, subi_same_lhs, multi_same_lhs) // changes on left hand side
  val rhs = Seq(LitInst(99), addi_same_rhs, subi_same_rhs, multi_same_rhs) // changes on right hand side

  /** Useful helper function to define essential eql=true test cases for instances */
  def eqls(instances: Seq[DataTypeInstance]): Seq[TestCase] = {
    var tcs: Seq[TestCase] = Seq.empty
    for (idx <- instances.indices) {
      tcs = tcs :+ EqualsBinaryMethodTestCase(Eql, instances(idx), instances(idx), result = true)
    }
    tcs
  }

  /** Useful helper function to ensure all other eql=false test cases for instances */
  def not_eqls(instances: Seq[DataTypeInstance]): Seq[TestCase] = {
    var tcs: Seq[TestCase] = Seq.empty
    for (idx <- instances.indices) {
      for (jdx <- instances.indices) {
        if (idx != jdx) {
          tcs = tcs :+ EqualsBinaryMethodTestCase(Eql, instances(idx), instances(jdx), result = false)
        }
      }
    }
    tcs
  }

  /**
   * Given THREE lists, produce all false test cases for binary operations which checks for the trio of mismatches:
   * Inst[i] with lhs[i] where lhs[i] perturbs just the left-hand side
   * Inst[i] with rhs[i] where rgs[i] perturbs just the Right-hand side
   * lhs[i] with rhs[i]
   */
  def struct_not_eqls(instances: Seq[DataTypeInstance], lhs: Seq[DataTypeInstance], rhs: Seq[DataTypeInstance]): Seq[TestCase] = {
    var tcs: Seq[TestCase] = Seq.empty
    for (idx <- instances.indices) {
      tcs = tcs :+ EqualsBinaryMethodTestCase(Eql, instances(idx), lhs(idx), result = false)
      tcs = tcs :+ EqualsBinaryMethodTestCase(Eql, instances(idx), rhs(idx), result = false)
      tcs = tcs :+ EqualsBinaryMethodTestCase(Eql, lhs(idx), rhs(idx), result = false)
    }
    tcs
  }

  def tests: Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, multi, Eval, M0.DoubleInst(6.0)),
    MultByTestCase(MultInst(LitInst(3.0), LitInst(2.0)), InstanceRep(LitInst(5.0)), DoubleInst(30.0)),
    MultByTestCase(MultInst(LitInst(-3.0), LitInst(2.0)), InstanceRep(LitInst(-5.0)), DoubleInst(30.0)),
  ) ++ eqls(all_instances) ++ not_eqls(all_instances) ++ struct_not_eqls(all_instances, lhs, rhs)
}
