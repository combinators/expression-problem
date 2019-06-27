package org.combinators.ep.domain.math    /*DD:LI:AI*/

import org.combinators.ep.domain.Evolution
import org.combinators.ep.domain.tree.{Leaf, Node}

trait M8 extends Evolution {
  self: M0 with M1 with M2 with M3 with M4 with M5 with M6 with M7 =>
  val domain:MathDomain
  import domain._

  // SquareRoot of inner value, and an operation Find that counts the number
  case object Power extends Binary("Power")
  case object Copy extends ProducerOperation("copy")

  val m8 = Model("m8", Seq(Power), Seq(Copy), last = m7)

  override def getModel = m8

  val m8_1 = new BinaryInst(Power, LitInst(6.0), LitInst(2.0))
  val m8_2 = new BinaryInst(Power, LitInst(25.0), LitInst(-0.5))
  val m8_3 = new BinaryInst(Power, LitInst(10.0), LitInst(0.0))

  val m8_tree = new BinaryInst(Mult, LitInst(2.0), new UnaryInst(Sqrt, LitInst(7.0)))
  val m8_tree1 = new Node(Seq(new Leaf(2.0), new Node(Seq(new Leaf(7.0)), Sqrt.name.hashCode)), Mult.name.hashCode)

  def M8_tests:Seq[TestCase] = Seq(
    EqualsTestCase(m8_1, Eval, ExistsInstance(Double)(36.0)),
    EqualsTestCase(m8_2, PrettyP, ExistsInstance(String)("Power(25.0,-0.5)")),
    EqualsTestCase(m8_3, Eval, ExistsInstance(Double)(1.0)),

    EqualsCompositeTestCase(m8_3, Seq((Simplify, Seq.empty), (PrettyP, Seq.empty)), ExistsInstance(String)("1.0")),
    EqualsCompositeTestCase(m8_tree, Seq((Copy, Seq.empty), (AsTree, Seq.empty)), ExistsInstance(TreeType)(m8_tree1)),

  )
}
