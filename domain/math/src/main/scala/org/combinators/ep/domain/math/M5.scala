package org.combinators.ep.domain.math      /*DD:LI:AI*/

import org.combinators.cogen.InstanceRep
import org.combinators.ep.domain.instances.DataTypeInstanceRep
import org.combinators.cogen.{TestCase, TypeRep}
import org.combinators.ep.domain.*
import org.combinators.ep.domain.abstractions.{DomainTpeRep, EqualsCompositeTestCase, EqualsTestCase, Operation}
import org.combinators.ep.domain.instances.DataTypeInstance
import org.combinators.ep.domain.tree.*
import org.combinators.ep.domain.math.M0.{Add, AddInst, DoubleInst, Lit, LitInst}
import org.combinators.ep.domain.math.M1.{Sub, SubInst}
import org.combinators.ep.domain.math.M2.{PrettyP, StringInst}
import org.combinators.ep.domain.math.M3.{Divd, DivdInst, Mult, MultInst, Neg, NegInst}
import org.combinators.ep.domain.math.M4.Simplify

object M5 extends Evolution {
  override implicit def getModel:GenericModel = M4.getModel.evolve("m5", Seq.empty, Seq(Operation.asTree, Identifier))
  lazy val Identifier = Operation("id", TypeRep.Int)

  val m5_s1 = SubInst(LitInst(1.0), LitInst(976.0))
  val m5_s2 = AddInst(LitInst(1.0), LitInst(976.0))
  val m5_s3 = SubInst(LitInst(1.0), LitInst(976.0))

  val m5_all = SubInst(
    NegInst(LitInst(2.0)), // Sub-Left
    MultInst(             // Sub-Right
      SubInst(LitInst(1.0), LitInst(976.0)),   // Mult-Left
      AddInst(                                // Mult-Right
        MultInst(LitInst(1.0), LitInst(976.0)),
        DivdInst(LitInst(1.0), LitInst(3.0)))))

  val tree_m5_all =
    Node(Sub.name.hashCode,
      Seq(
        Node(Neg.name.hashCode, Seq(Node(Lit.name.hashCode, Seq(Leaf(DoubleInst(2.0)))))), // Sub-Left
        Node(Mult.name.hashCode,
          Seq(
            Node(Sub.name.hashCode,
              Seq(
                Node(Lit.name.hashCode, Seq(Leaf(DoubleInst(1.0)))),
                Node(Lit.name.hashCode, Seq(Leaf(DoubleInst(976.0))))
              )), // Mult-Left
            Node(Add.name.hashCode,
              Seq(
                Node(Mult.name.hashCode,
                  Seq(
                    Node(Lit.name.hashCode, Seq(Leaf(DoubleInst(1.0)))),
                    Node(Lit.name.hashCode, Seq(Leaf(DoubleInst(976.0))))
                  )),
                Node(Divd.name.hashCode,
                  Seq(
                    Node(Lit.name.hashCode, Seq(Leaf(DoubleInst(1.0)))),
                    Node(Lit.name.hashCode, Seq(Leaf(DoubleInst(3.0))))
                  ))
              ))
          )) // Mult-Right
      ))

  val m5_s4 = MultInst(MultInst(LitInst(2.0), LitInst(1.0)),
                                   AddInst(LitInst(0.0), LitInst(7.0)))


  val treeSimplified =
    Node(Mult.name.hashCode,
      Seq(
        Node(Lit.name.hashCode, Seq(Leaf(DoubleInst(2.0)))),
        Node(Lit.name.hashCode, Seq(Leaf(DoubleInst(7.0))))
      ))
  /**
    * Special test case for same queries.
    *
    * Validates that calling AsTree on inst1 yields the tree called from AsTree on inst2
    */
  /** Models a test case which applies operation `op` to `domainObject` and `params`, expecting a result
   * equal to `expected`. */
  case class SameTestCase(
         inst1: DataTypeInstance,
         inst2: DataTypeInstance,
         expected: Boolean,
         params: InstanceRep*
       ) extends TestCase

  override def allTests: Map[GenericModel, Seq[TestCase]] = allPastTests(M4)

  def tests:Seq[TestCase] = Seq(
    SameTestCase(m5_s1, m5_s2, expected = false),
    SameTestCase(m5_s1, m5_s3, expected = true),
    SameTestCase(m5_all, m5_all, expected = true),

    EqualsTestCase(getModel.baseDataType, m5_all, Operation.asTree, InstanceRep(DomainTpeRep.Tree)(tree_m5_all)),
    EqualsCompositeTestCase(getModel.baseDataType, m5_all, StringInst("(-2.0-((1.0-976.0)*((1.0*976.0)+(1.0/3.0))))"), (PrettyP, Seq.empty)),

    EqualsCompositeTestCase(getModel.baseDataType, m5_s4, InstanceRep(DomainTpeRep.Tree)(treeSimplified), (Simplify, Seq.empty), (Operation.asTree, Seq.empty)),
  )
}
