package org.combinators.ep.domain.tree   /*DI:LI:AI*/

class Leaf(val value:Any) extends Tree {

  override def asLeaf() : Option[Leaf] = Some(this)
}
