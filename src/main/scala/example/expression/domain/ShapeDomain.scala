package example.expression.domain

/**
  * Shape domain as suitable for
  *
  * Synthesizing Object-Oriented and Functional Design to Promote Re-Use
  * Shriram Krishnamurthi, Matthias Felleisen, Daniel Friedman
  *
  * https://www.cs.rice.edu/~cork/teachjava/2003/readings/visitor1.pdf
  */
trait ShapeDomain extends BaseDomain with ModelDomain {

  case object Shape extends TypeRep {
    override def name: String = "Shape"
  }
  type BaseTypeRep = Shape.type
  val baseTypeRep:BaseTypeRep = Shape

  // standard attributes for domain. As new ones are defined, place here
  object attributes {
    val side:String   = "side"
    val point:String  = "point"
    val trans:String  = "trans"
    val radius:String = "radius"
    val shape:String  = "shape"
    val pct:String    = "pct"
  }

  // s0:model evolution.
  // -------------------
  case object Point extends TypeRep
  case object Double extends TypeRep
  case object Boolean extends TypeRep

  case object Square extends Atomic("Square", Seq(Attribute(attributes.side, Double)))
  case object Circle extends Atomic("Circle", Seq(Attribute(attributes.radius, Double)))
  case object Translate extends Atomic("Translate",
    Seq(Attribute(attributes.trans, Point), Attribute(attributes.shape, Shape)))
  case object ContainsPt extends Operation("containsPt", Some(Boolean), Seq((attributes.point, Point)))
  val s0 = Model("s0", Seq(Square,Circle,Translate), Seq(ContainsPt))

  class SquareInst(d:Double) extends AtomicInst(Square, Some(d))
  class CircleInst(d:Double) extends AtomicInst(Circle, Some(d))
  class TranslateInst(pt:(Double,Double), s:AtomicInst) extends AtomicInst(Translate, Some((pt,s)))

  // s1:model evolution (add operation)
  // ----------------------------------
  case object Shrink extends Operation("shrink", Some(Shape), Seq((attributes.pct, Double)))
  val s1 = Model("s1", Seq.empty, Seq(Shrink), s0)

  // s2:model evolution (add datatype) -- NOT YET IMPLEMENTED
  // ---------------------------------------------------------
  case object Composite extends Atomic("Composite",
    Seq(Attribute(base.left, Shape), Attribute(base.right, Shape)))
  val s2 = Model ("s2", Seq(Composite), Seq.empty, s1)
}
