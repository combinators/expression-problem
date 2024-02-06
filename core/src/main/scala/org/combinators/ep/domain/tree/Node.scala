package org.combinators.ep.domain.tree   /*DI:LI:AI*/

class Node(val children:Seq[Tree], val label:Int) extends Tree {

  // copy elements
  val subtrees:Seq[Tree] = children.map {x => x}

  override def asNode():Option[Node] = Some(this)
}

