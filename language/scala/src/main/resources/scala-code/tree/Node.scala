package tree

class Node(val children:Seq[Tree], val label:Int) extends Tree {

  // copy elements
  val subtrees:Seq[Tree] = children.map {x => x}

  override def canEqual(a: Any) = a.isInstanceOf[Node]

  /** link in with default equals. */
  override def equals(tree: Any): Boolean = {
    tree match {
      case o: Node => o.canEqual(this) && same(o)

      case _ => false
    }
  }

  override def asNode():Option[Node] = Some(this)
}
