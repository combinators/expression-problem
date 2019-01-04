package example.expression.j  /*DD:LD:AI*/

import com.github.javaparser.ast.body.MethodDeclaration
import example.expression.domain.{Evolution, ShapeDomain}
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait s0 extends Evolution with JavaGenerator with JUnitTestGenerator {
  val domain:ShapeDomain

  // standard attributes for domain. As new ones are defined, place in respective traits
  val side:String   = "side"
  val radius:String = "radius"
  val trans:String  = "trans"
  val shape:String  = "shape"
  val point:String  = "point"
  val pct:String    = "pct"

  case object Point extends domain.TypeRep
  case object Double extends domain.TypeRep
  case object Boolean extends domain.TypeRep

  case object Square extends domain.Atomic("Square", Seq(domain.Attribute(side, Double)))
  case object Circle extends domain.Atomic("Circle", Seq(domain.Attribute(radius, Double)))
  case object Translate extends domain.Atomic("Translate",
    Seq(domain.Attribute(trans, Point), domain.Attribute(shape, domain.Shape)))
  case object ContainsPt extends domain.Operation("containsPt", Some(Boolean), Seq((point, Point)))
  val s0 = domain.Model("s0", Seq(Square,Circle,Translate), Seq(ContainsPt))

  class SquareInst(d:Double) extends domain.AtomicInst(Square, Some(d))
  class CircleInst(d:Double) extends domain.AtomicInst(Circle, Some(d))
  class TranslateInst(pt:(Double,Double), s:domain.AtomicInst) extends domain.AtomicInst(Translate, Some((pt,s)))

  override def getModel = s0

  /** E0 Introduces the concept a Double type, used for the 'Eval' operation. */
  abstract override def typeConverter(tpe:domain.TypeRep) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case Double => Java("Double").tpe()
      case Point => Java("java.awt.geom.Point2D.Double").tpe()
      case Boolean => Java("Boolean").tpe()
      case _ => super.typeConverter(tpe)
    }
  }

  /** Eval operation needs to provide specification for current datatypes, namely Lit and Add. */
  abstract override def logic(exp:domain.Atomic)(op:domain.Operation): Seq[Statement] = {
    val subs:Map[String,Expression] = subExpressions(exp).asInstanceOf[Map[String,Expression]]

    // generate the actual body
    op match {
      case ContainsPt =>
        exp match {
          case Circle =>
            Java(s"return Math.sqrt(point.x*point.x + point.y*point.y) <= ${subs(radius)};").statements

          case Square =>
            Java(s"return (Math.abs(point.x) <= ${subs(side)}/2 && Math.abs(point.y) <= ${subs(side)}/2);").statements

          case Translate => {
            Java(
              s"""
                 |// first adjust
                 |java.awt.geom.Point2D.Double t = new java.awt.geom.Point2D.Double(point.x - ${subs(trans)}.x, point.y - ${subs(trans)}.y);
                 |return ${dispatch(subs(shape), ContainsPt, Java("t").expression[Expression]())};
                 |
               """.stripMargin).statements()
          }
        }

      case _ => super.logic(exp)(op)
    }
  }

  /** Convert a test instance into a Java Expression for instantiating that instance. */
  override def convert(inst:domain.AtomicInst) : Expression = {
    val name = inst.e.name
    inst match {
      case ti:TranslateInst => {
        val tuple = ti.i.get.asInstanceOf[((Double,Double),domain.AtomicInst)]
        val pt = s"new java.awt.geom.Point2D.Double(${tuple._1._1}, ${tuple._1._2})"

        Java(s"new $name($pt, ${convert(tuple._2)})").expression()
      }

      case _ => super.convert(inst)
    }
  }

  abstract override def testGenerator: Seq[MethodDeclaration] = {

    val s1 = new SquareInst(5.0)
    val c1 = new CircleInst(5.0)  // instances.ExpInst
    val p1 = Java("new java.awt.geom.Point2D.Double(2, 2)").expression[Expression]()
    val p2 = Java("new java.awt.geom.Point2D.Double(8, 0)").expression[Expression]()

    val t1 = new TranslateInst((5.0, 7.0), s1)
    val t2 = new TranslateInst((2.0, -9.0), t1)

    super.testGenerator ++ Java(
      s"""
         |public void test() {
         |   assertTrue(${dispatch(convert(s1), ContainsPt, p1)});
         |   assertFalse(${dispatch(convert(c1), ContainsPt, p2)});
         |
         |   assertFalse(${dispatch(convert(t1), ContainsPt, p1)});
         |   assertFalse(${dispatch(convert(t2), ContainsPt, p1)});
         |   assertTrue(${dispatch(convert(t2), ContainsPt, p2)});
         |
         |}""".stripMargin).methodDeclarations()
  }
}
