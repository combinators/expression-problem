package org.combinators.ep.domain.shape

import org.combinators.ep.domain.{BaseDomain, Evolution}

trait S0 extends Evolution {

  val domain:ShapeDomain
  import domain._

  // standard attributes for domain. As new ones are defined, place in respective traits
  val side = Attribute("side", Double)
  val radius = Attribute("radius", Double)
  val x = Attribute("x", Double)
  val y = Attribute("y", Double)
  val trans = Attribute("trans", Point2D)
  val shape = Attribute("shape", domain.Shape)
  val point = Parameter("point", Point2D)
  val pct = Parameter("pct", Double)

  case object Double extends domain.TypeRep {
    type scalaInstanceType = scala.Double
  }
  case object Boolean extends domain.TypeRep {
    type scalaInstanceType = scala.Boolean
  }
  case object Point2D extends domain.TypeRep {
    type scalaInstanceType = (scala.Double, scala.Double)
  }

  case object Square extends domain.Atomic("Square", Seq(side))
  case object Circle extends domain.Atomic("Circle", Seq(radius))
  case object Point extends domain.Atomic("Point", Seq(x, y))
  case object Translate extends domain.DataType("Translate", Seq(trans, shape))

  case object ContainsPt extends domain.Operation("containsPt", Some(Boolean), Seq(point))

  case class SquareInst(d:scala.Double) extends domain.AtomicInst(Square, ExistsInstance(Double)(d))
  case class CircleInst(d:scala.Double) extends domain.AtomicInst(Circle, ExistsInstance(Double)(d))
  case class PointInst(x:scala.Double, y:scala.Double) extends domain.AtomicInst(Point, ExistsInstance(Point2D)((x,y)))
  case class TranslateInst(pt:(scala.Double,scala.Double), s:Inst) extends domain.Inst {
    val e: DataType = Translate
    val ei: ExistsInstance = ExistsInstance(Point2D)(pt)
    override def name: String = e.name
  }

  val s0 = domain.Model("s0", Seq(Square,Circle,Translate), Seq(ContainsPt))
  override def getModel:Model = s0

  val sq1 = new SquareInst(5.0)
  val c1 = new CircleInst(5.0)
  val p1:(Double,Double) = (2.0, 2.0)
  val p2:(Double,Double) = (8.0, 0.0)

  val t1 = new TranslateInst((5.0, 7.0), sq1)
  val t2 = new TranslateInst((2.0, -9.0), t1)

  /**
    * Special test case for contains queries.
    *
    * Validates calling containsPt returns true or false
    */
  case class ContainsTestCase(inst:domain.Inst, pt:(Double,Double), result:Boolean)
    extends domain.TestCase {
    val pti: ExistsInstance = ExistsInstance(Point2D)(pt)
  }

  def S0_tests:Seq[TestCase] = Seq(
    ContainsTestCase(sq1, p1, result = true),
    ContainsTestCase(c1, p2, result = false),

    ContainsTestCase(t1, p1, result = false),
    ContainsTestCase(t2, p1, result = false),
    ContainsTestCase(t2, p2, result = true),
  )

}
