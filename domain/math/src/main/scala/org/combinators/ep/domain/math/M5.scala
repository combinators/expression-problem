package org.combinators.ep.domain.math      /*DD:LI:AI*/

import org.combinators.ep.domain.Evolution
import org.combinators.ep.domain.tree._

trait M5 extends Evolution {
  self: M0 with M1 with M2 with M3 with M4 =>
  val domain:MathDomain
  import domain._

  // m5:model evolution.
  // -------------------
  // Represent structure as a tree
  case object Identifier extends Operation("id", Some(Int))

  val m5 = Model("m5", Seq.empty, Seq(domain.AsTree, Identifier), last = m4)
  override def getModel = m5

  // Tests
  val m5_s1 = new BinaryInst(Sub, LitInst(1.0), LitInst(976.0))
  val m5_s2 = new BinaryInst(Add, LitInst(1.0), LitInst(976.0))
  val m5_s3 = new BinaryInst(Sub, LitInst(1.0), LitInst(976.0))

  val m5_all = new BinaryInst(Sub,
    new UnaryInst(Neg, LitInst(2.0)), // Sub-Left
    new BinaryInst(Mult,              // Sub-Right
      new BinaryInst(Sub, LitInst(1.0), LitInst(976.0)),   // Mult-Left
      new BinaryInst(Add,                                  // Mult-Right
        new BinaryInst(Mult, LitInst(1.0), LitInst(976.0)),
        new BinaryInst(Divd,  LitInst(1.0), LitInst(3.0)))))

  val tree_m5_all =
    new Node(Seq(new Node(Seq(new Leaf(2.0)), Neg.name.hashCode), // Sub-Left
                 new Node(Seq(new Node(Seq(new Leaf(1.0), new Leaf(976.0)), Sub.name.hashCode), // Mult-Left
                              new Node(Seq(new Node(Seq(new Leaf(1.0), new Leaf(976.0)), Mult.name.hashCode),
                                           new Node(Seq(new Leaf(1.0), new Leaf(3.0)), Divd.name.hashCode)),
                                       Add.name.hashCode)),  // Mult-Right
                   Mult.name.hashCode)),   // Sub-Right
              Sub.name.hashCode)

  val m5_s4 = new BinaryInst(Mult, new BinaryInst(Mult, LitInst(2.0), LitInst(1.0)),
                                   new BinaryInst(Add, LitInst(0.0), LitInst(7.0)))


  new Leaf(2.0)

  val treeSimplified = new Node(Seq(new Leaf(2.0), new Leaf(7.0)), Mult.name.hashCode)
  /**
    * Special test case for same queries.
    *
    * Validates that calling AsTree on inst1 yields the tree called from AsTree on inst2
    */
  case class SameTestCase(inst1:domain.Inst, inst2:domain.Inst, result:Boolean)
    extends domain.TestCase

  def M5_tests:Seq[TestCase] = Seq(
    SameTestCase(m5_s1, m5_s2, result=false),
    SameTestCase(m5_s1, m5_s3, result=true),
    SameTestCase(m5_all, m5_all, result=true),
    EqualsTestCase(m5_all, AsTree, ExistsInstance(TreeType)(tree_m5_all)),
    EqualsCompositeTestCase(m5_all, Seq((PrettyP, Seq.empty)), ExistsInstance(String)("(-2.0-((1.0-976.0)*((1.0*976.0)+(1.0/3.0))))")),

    EqualsCompositeTestCase(m5_s4, Seq((Simplify, Seq.empty), (AsTree, Seq.empty)), ExistsInstance(TreeType)(treeSimplified)),
  )
}
