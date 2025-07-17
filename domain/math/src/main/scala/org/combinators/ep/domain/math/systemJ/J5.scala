package org.combinators.ep.domain.math.systemJ    /*DD:LI:AI*/

import org.combinators.cogen.InstanceRep
import org.combinators.cogen.TypeRep
import org.combinators.ep.domain.instances.DataTypeInstanceRep
import org.combinators.cogen.TestCase
import org.combinators.ep.domain.abstractions._
import org.combinators.ep.domain.instances.DataTypeInstance
import org.combinators.ep.domain.math.M0.{Add, AddInst, LitInst, addi}
import org.combinators.ep.domain.math.M5
import org.combinators.ep.domain.math.systemJ.J3.all_instances
import org.combinators.ep.domain.{Evolution, GenericModel}

object J5 extends Evolution {
  override implicit def getModel: GenericModel = J4.getModel.evolve("j5", Seq.empty, Seq(Equals))

  lazy val Equals = Operation("equal_to", TypeRep.Boolean, Seq(Parameter("other", DomainTpeRep.DataType(M5.getModel.baseDataType))))

  object EqualsBinaryMethodTestCase {
    def apply(op: Operation, instance: DataTypeInstance, instance1: DataTypeInstance, result: Boolean): TestCase = {
      EqualsTestCase(
        M5.getModel.baseDataType,
        instance,
        op,
        InstanceRep(TypeRep.Boolean)(result),
        DataTypeInstanceRep.apply(instance1)(getModel)
      )
    }
  }

  /** Useful helper function to define essential eql=true test cases for instances */
  def op_equals(instances: Seq[DataTypeInstance]): Seq[TestCase] = {
    var tcs: Seq[TestCase] = Seq.empty
    for (idx <- instances.indices) {
      tcs = tcs :+ EqualsBinaryMethodTestCase(Equals, instances(idx), instances(idx), result = true)
    }
    tcs
  }

  /** Useful helper function to ensure all other eql=false test cases for instances */
  def op_not_equals(instances: Seq[DataTypeInstance]): Seq[TestCase] = {
    var tcs: Seq[TestCase] = Seq.empty
    for (idx <- instances.indices) {
      for (jdx <- instances.indices) {
        if (idx != jdx) {
          tcs = tcs :+ EqualsBinaryMethodTestCase(Equals, instances(idx), instances(jdx), result = false)
        }
      }
    }
    tcs
  }

  override def allTests: Map[GenericModel, Seq[TestCase]] = allPastTests(J4)

  def tests: Seq[TestCase] = Seq(
    PerformanceTestCase(
      11,
      8,
      Equals,
      getModel.baseDataType,
      addi, // first, base instance

      // initial parameter to use when testing equals
      Seq(DataTypeInstanceRep(AddInst(LitInst(1.0), LitInst(2.0)))),

      // function tells how InstanceRep parameters evolve with each iteration
      // Seq[InstanceRep] => Seq[InstanceRep]
      params => params.map(param =>
        param.inst match {
          case i: InstanceRep => DataTypeInstanceRep(DataTypeInstance(Add, Seq(i, i)))
          case _ => param
        }),

      inst => AddInst(inst, inst) // function tells how objects evolve with each iteration
    )
  ) ++ op_equals(all_instances) ++ op_not_equals(all_instances)

}
