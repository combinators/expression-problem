package example.expression.j

import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.expr.Expression
import com.github.javaparser.ast.stmt.Statement
import example.expression.domain.ShapeDomain
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait s0 extends AbstractGenerator with TestGenerator {
  val domain:ShapeDomain

  /** E0 Introduces the concept a Double type, used for the 'Eval' operation. */
  abstract override def typeGenerator(tpe:domain.types.Types) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case domain.Double => Java("Double").tpe()
      case domain.Point => Java("java.awt.geom.Point2D.Double").tpe()
      case domain.Boolean => Java("Boolean").tpe()
      case _ => super.typeGenerator(tpe)
    }
  }

  /** Eval operation needs to provide specification for current datatypes, namely Lit and Add. */
  abstract override def methodBodyGenerator(exp:domain.expressions.Exp)(op:domain.Operation): Seq[Statement] = {
    val subs:Map[String,Expression] = subExpressions(exp)

    // generate the actual body
    op match {
      case domain.ContainsPt =>
        exp match {
          case domain.Circle =>
            Java(
              s"""
                 |return Math.sqrt(point.x*point.x + point.y*point.y) <= ${subs(domain.attributes.radius)};
               """.stripMargin).statements()

          case domain.Square =>
            Java(
              s"""
                 |return (Math.abs(point.x) <= ${subs(domain.attributes.side)}/2 && Math.abs(point.y) <= ${subs(domain.attributes.side)}/2);
               """.stripMargin).statements()


            // return ${recurseOnWithParams(convert(s1, domain.emptyModel()), domain.ContainsPt, Java("t").expression[Expression]())});

            //return ${subs(domain.attributes.shape)}.${op.name}(t);
          case domain.Translate => {
            Java(
              s"""
                 |// first adjust
                 |java.awt.geom.Point2D.Double t = new java.awt.geom.Point2D.Double(point.x - ${subs(domain.attributes.trans)}.x, point.y - ${subs(domain.attributes.trans)}.y);
                 |return ${recurseOn(subs(domain.attributes.shape), domain.ContainsPt, Java("t").expression[Expression]())};
                 |
               """.stripMargin).statements()
          }
        }

      case _ => super.methodBodyGenerator(exp)(op)
    }
  }

  /** Convert a test instance into a Java Expression for instantiating that instance. */
  override def convert(inst:domain.ExpInst, model:domain.Model) : Expression = {
    val name = inst.e.name
    inst match {
      case ti:domain.TranslateInst => {
        val tuple = ti.i.get.asInstanceOf[((Double,Double),domain.ExpInst)]
        val pt = s"new java.awt.geom.Point2D.Double(${tuple._1._1}, ${tuple._1._2})"

        Java(s"new $name($pt, ${convert(tuple._2, model)})").expression()
      }

      case _ => super.convert(inst, model)
    }
  }

  abstract override def testGenerator(model:domain.Model): Seq[MethodDeclaration] = {

    val s1 = new domain.SquareInst(5.0)
    val c1 = new domain.CircleInst(5.0)  // instances.ExpInst
    val p1 = Java("new java.awt.geom.Point2D.Double(2, 2)").expression[Expression]()
    val p2 = Java("new java.awt.geom.Point2D.Double(8, 0)").expression[Expression]()

    val t1 = new domain.TranslateInst((5.0, 7.0), s1)
    val t2 = new domain.TranslateInst((2.0, -9.0), t1)

    super.testGenerator(model.last) ++ Java(
      s"""
         |public void test() {
         |   assertTrue(${recurseOn(convert(s1, model), domain.ContainsPt, p1)});
         |   assertFalse(${recurseOn(convert(c1, model), domain.ContainsPt, p2)});
         |
         |   assertFalse(${recurseOn(convert(t1, model), domain.ContainsPt, p1)});
         |   assertFalse(${recurseOn(convert(t2, model), domain.ContainsPt, p1)});
         |   assertTrue(${recurseOn(convert(t2, model), domain.ContainsPt, p2)});
         |
         |}""".stripMargin).methodDeclarations()
  }

}

