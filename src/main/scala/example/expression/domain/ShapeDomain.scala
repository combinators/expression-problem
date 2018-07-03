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
  case object Point extends Types
  case object Double extends Types
  case object Boolean extends Types

  case object Square extends expressions.Exp("Square", Seq(Attribute(attributes.side, Double)))
  case object Circle extends expressions.Exp("Circle", Seq(Attribute(attributes.radius, Double)))
  case object Translate extends expressions.Exp("Translate",
    Seq(Attribute(attributes.trans, Point), Attribute(attributes.shape, Exp)))
  case object ContainsPt extends Operation("containsPt", Some(Boolean), (attributes.point, Point))
  val s0 = Model("s0", Seq(Square,Circle,Translate), Seq(ContainsPt))

  class SquareInst(d:Double) extends ExpInst(Square, Some(d))
  class CircleInst(d:Double) extends ExpInst(Circle, Some(d))
  class TranslateInst(pt:(Double,Double), s:ExpInst) extends ExpInst(Translate, Some((pt,s)))

  // s1:model evolution (add operation)
  // ----------------------------------
  case object Shrink extends Operation("shrink", Some(Exp), (attributes.pct, Double))
  val s1 = Model("s1", Seq.empty, Seq(Shrink), s0)

  // s2:model evolution (add datatype)
  // ---------------------------------
  case object Composite extends expressions.Exp("Composite",
    Seq(Attribute(base.left, Exp), Attribute(base.right, Exp)))
  val s2 = Model ("s2", Seq(Composite), Seq.empty, s1)
}
