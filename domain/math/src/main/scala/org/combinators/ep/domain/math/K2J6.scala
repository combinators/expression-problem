package org.combinators.ep.domain.math     /*DD:LI:AI*/

import org.combinators.ep.domain._
import org.combinators.ep.domain.abstractions.{EqualsCompositeTestCase, EqualsTestCase, TestCase}
import org.combinators.ep.domain.instances.InstanceRep
import org.combinators.ep.domain.math.J1.subi
import org.combinators.ep.domain.math.J2.{MultInst, eqls, multi, not_eqls, struct_not_eqls}
import org.combinators.ep.domain.math.J3.{Divd, DivdInst, NegInst, PrettyP, StringInst, divdi, negi}
import org.combinators.ep.domain.math.K1.{PowerInst, powi, powi_same_lhs, powi_same_rhs}
import org.combinators.ep.domain.math.K2.{Collect, ListDoubleInst, Simplify}
import org.combinators.ep.domain.math.J5.{op_equals, op_not_equals}
import org.combinators.ep.domain.math.J6.PowBy
import org.combinators.ep.domain.math.M0.{AddInst, DoubleInst, Eval, Lit, LitInst, addi, liti}
import org.combinators.ep.domain.tree.{Leaf, Node}

object K2J6 extends Evolution {
  override implicit def getModel:GenericModel = J6.getModel.merge("k2j6", Seq.empty, Seq.empty, Seq(K2.getModel))

  val all_instances = J3.all_instances ++ Seq(powi)
  val lhs           = J3.lhs ++ Seq(powi_same_lhs)   // changes on left hand side
  val rhs           = J3.rhs ++ Seq(powi_same_rhs)   // changes on right hand side

  val tree_node = Node(Divd.name.hashCode,
    Seq(
      Node(Lit.name.hashCode, Seq(Leaf(DoubleInst(1.0)))),
      Node(Lit.name.hashCode, Seq(Leaf(DoubleInst(3.0))))
    ))

  // testing
  def tests: Seq[TestCase] = Seq(
    EqualsTestCase(getModel.baseDataType, powi, PrettyP, StringInst("(3.0^5.0)")),

    EqualsTestCase(getModel.baseDataType, AddInst(powi, divdi), Collect, ListDoubleInst(Seq(3.0, 5.0, 10.0, 5.0))),
    EqualsTestCase(getModel.baseDataType, PowerInst(addi, multi), Collect, ListDoubleInst(Seq(1.0, 2.0, 2.0, 3.0))),
    EqualsTestCase(getModel.baseDataType, DivdInst(negi, liti), Collect, ListDoubleInst(Seq(3.0, 5.0))),
    EqualsTestCase(getModel.baseDataType, NegInst(subi), Collect, ListDoubleInst(Seq(1.0, 2.0))),

    // in CoCo this leads to an invocation of .equals which defaults to java.lang.equals, so this won't work in some approaches that rely on default
    // implementations in interfaces, which will take second place to default java.lang.Object#equals. Works for OO-Traditional; breaks for Coco.
    // EqualsTestCase(getModel.baseDataType, PowerInst(LitInst(2.0), LitInst(3.0)), PowBy, InstanceRep(PowerInst(PowerInst(LitInst(2.0), LitInst(3.0)), LitInst(2.0))),
    //  InstanceRep(LitInst(2.0))),  // 8^2 == 64
    // Use This Instead as a substitute for above
    EqualsCompositeTestCase(getModel.baseDataType, PowerInst(LitInst(2.0), LitInst(3.0)), DoubleInst(64.0), (PowBy, Seq(InstanceRep(LitInst(2.0)))), (Eval, Seq.empty)),

    EqualsCompositeTestCase(getModel.baseDataType, DivdInst(LitInst(5.0), LitInst(5.0)), DoubleInst(1.0), (Simplify, Seq.empty), (Eval, Seq.empty)),
    EqualsCompositeTestCase(getModel.baseDataType, DivdInst(LitInst(5.0), LitInst(1.0)), DoubleInst(5.0), (Simplify, Seq.empty), (Eval, Seq.empty)),
    EqualsCompositeTestCase(getModel.baseDataType, DivdInst(LitInst(0.0), LitInst(1.0)), DoubleInst(0.0), (Simplify, Seq.empty), (Eval, Seq.empty)),
    EqualsCompositeTestCase(getModel.baseDataType, DivdInst(LitInst(5.0), LitInst(-5.0)), DoubleInst(-1.0), (Simplify, Seq.empty), (Eval, Seq.empty)),

    EqualsCompositeTestCase(getModel.baseDataType, MultInst(LitInst(5.0), LitInst(0.0)), DoubleInst(0.0), (Simplify, Seq.empty), (Eval, Seq.empty)),
    EqualsCompositeTestCase(getModel.baseDataType, MultInst(LitInst(0.0), LitInst(5.0)), DoubleInst(0.0), (Simplify, Seq.empty), (Eval, Seq.empty)),

    EqualsCompositeTestCase(getModel.baseDataType, NegInst(LitInst(0.0)), DoubleInst(0.0), (Simplify, Seq.empty), (Eval, Seq.empty)),
    EqualsCompositeTestCase(getModel.baseDataType, NegInst(LitInst(3.0)), DoubleInst(-3.0), (Simplify, Seq.empty), (Eval, Seq.empty)),

    EqualsCompositeTestCase(getModel.baseDataType, divdi, M0.DoubleInst(2.0) , (Simplify, Seq.empty), (Eval, Seq.empty)),
    EqualsCompositeTestCase(getModel.baseDataType, negi, M0.DoubleInst(-3.0) , (Simplify, Seq.empty), (Eval, Seq.empty)),

    // Cannot write a test case like this, because Boolean is not in the domain! We could seek to add it as a type (whose eval is 0.0 or 1.0)
//    EqualsCompositeTestCase(getModel.baseDataType, DivdInst(LitInst(5.0), LitInst(5.0)), DoubleInst(1.0),
//      (Simplify, Seq.empty), (Eql, Seq(InstanceRep(LitInst(1.0)))), (Eval, Seq.empty)),

  ) ++ eqls(all_instances) ++ not_eqls(all_instances) ++ struct_not_eqls(all_instances, lhs, rhs) ++ op_equals(all_instances) ++ op_not_equals(all_instances)
}

