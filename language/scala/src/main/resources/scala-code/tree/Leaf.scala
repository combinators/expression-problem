package tree

class Leaf(val value:Any) extends Tree {

  override def canEqual(a: Any) = a.isInstanceOf[Leaf]

  override def asLeaf() : Option[Leaf] = Some(this)

  /** link in with default equals. */
  override def equals(tree: Any): Boolean = {
    tree match {
      case o: Leaf => o.canEqual(this) && same(o)

      case _ => false
    }
  }
}
