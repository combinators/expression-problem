package tree

class Leaf(val value:Any) extends Tree {

  override def asLeaf() : Option[Leaf] = Some(this)
}
