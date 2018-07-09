package example.expression.j

import com.github.javaparser.ast.`type`.Type
import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.expr.Expression
import com.github.javaparser.ast.stmt.Statement
import example.expression.domain.{Evolution, ShapeDomain}
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait s0 extends Evolution with AbstractGenerator with TestGenerator {
  val domain:ShapeDomain

  case object Point extends domain.TypeRep
  case object Double extends domain.TypeRep
  case object Boolean extends domain.TypeRep

  case object Square extends domain.Atomic("Square", Seq(domain.Attribute(domain.attributes.side, Double)))
  case object Circle extends domain.Atomic("Circle", Seq(domain.Attribute(domain.attributes.radius, Double)))
  case object Translate extends domain.Atomic("Translate",
    Seq(domain.Attribute(domain.attributes.trans, Point), domain.Attribute(domain.attributes.shape, domain.Shape)))
  case object ContainsPt extends domain.Operation("containsPt", Some(Boolean), Seq((domain.attributes.point, Point)))
  val s0 = domain.Model("s0", Seq(Square,Circle,Translate), Seq(ContainsPt))

  class SquareInst(d:Double) extends domain.AtomicInst(Square, Some(d))
  class CircleInst(d:Double) extends domain.AtomicInst(Circle, Some(d))
  class TranslateInst(pt:(Double,Double), s:domain.AtomicInst) extends domain.AtomicInst(Translate, Some((pt,s)))

  override def getModel = s0

  /** E0 Introduces the concept a Double type, used for the 'Eval' operation. */
  abstract override def typeConverter(tpe:domain.TypeRep, covariantReplacement:Option[Type] = None) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case Double => Java("Double").tpe()
      case Point => Java("java.awt.geom.Point2D.Double").tpe()
      case Boolean => Java("Boolean").tpe()
      case _ => super.typeConverter(tpe, covariantReplacement)
    }
  }

  /** Eval operation needs to provide specification for current datatypes, namely Lit and Add. */
  abstract override def logic(exp:domain.Atomic)(op:domain.Operation): Seq[Statement] = {
    val subs:Map[String,Expression] = subExpressions(exp)

    // generate the actual body
    op match {
      case ContainsPt =>
        exp match {
          case Circle =>
            Java(
              s"""
                 |return Math.sqrt(point.x*point.x + point.y*point.y) <= ${subs(domain.attributes.radius)};
               """.stripMargin).statements()

          case Square =>
            Java(
              s"""
                 |return (Math.abs(point.x) <= ${subs(domain.attributes.side)}/2 && Math.abs(point.y) <= ${subs(domain.attributes.side)}/2);
               """.stripMargin).statements()


            // return ${recurseOnWithParams(convert(s1, domain.emptyModel()), domain.ContainsPt, Java("t").expression[Expression]())});

            //return ${subs(domain.attributes.shape)}.${op.name}(t);
          case Translate => {
            Java(
              s"""
                 |// first adjust
                 |java.awt.geom.Point2D.Double t = new java.awt.geom.Point2D.Double(point.x - ${subs(domain.attributes.trans)}.x, point.y - ${subs(domain.attributes.trans)}.y);
                 |return ${recurseOn(subs(domain.attributes.shape), ContainsPt, Java("t").expression[Expression]())};
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
         |   assertTrue(${recurseOn(convert(s1), ContainsPt, p1)});
         |   assertFalse(${recurseOn(convert(c1), ContainsPt, p2)});
         |
         |   assertFalse(${recurseOn(convert(t1), ContainsPt, p1)});
         |   assertFalse(${recurseOn(convert(t2), ContainsPt, p1)});
         |   assertTrue(${recurseOn(convert(t2), ContainsPt, p2)});
         |
         |}""".stripMargin).methodDeclarations()
  }
}
