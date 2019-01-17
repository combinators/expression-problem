package example.expression.j    /*DD:LD:AI*/

import com.github.javaparser.ast.body.MethodDeclaration
import example.expression.domain.{S1, ShapeDomain}
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait s1 extends JavaGenerator with JUnitTestGenerator with S1 { self:s0 =>
  val domain:ShapeDomain

  /** Eval operation needs to provide specification for current datatypes, namely Lit and Add. */
  abstract override def logic(exp:domain.Atomic, op:domain.Operation): Seq[Statement] = {
    val subs:Map[String,Expression] = subExpressions(exp)

    // generate the actual body
    op match {
      case Shrink =>
        exp match {
          case Circle =>
            Java(
              s"""
                 |double shrunkRadius = ${subs(radius)}*pct;
                 return ${inst(Circle, Java("shrunkRadius").expression[Expression]())};
               """.stripMargin).statements()

          case Square => {
            val str =
              s"""
                 |double shrunkSide = ${subs(side)}*pct;
                 |return ${inst(Square, Java("shrunkSide").expression[Expression]())};
               """.stripMargin
            println(str)
            Java(str).statements()
          }

          case Translate => {
            val disp = dispatch(subs(shape), op, expression("pct"))
            Java(
              s"""
                 |return ${inst(Translate, subs(trans),disp)};
                 |
               """.stripMargin).statements()
          }
        }

      case _ => super.logic(exp, op)
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

    val s1 = new SquareInst(8.0)
    val d1 = Java("0.5").expression[Expression]()

    super.testGenerator ++ Java(
      s"""
         |public void test() {
         |   // without access to the attributes, we can't write meaningful attribute test cases.
         |   assertNotNull( ${dispatch(convert(s1), Shrink, d1)});
         |
         |   // Handle collect checks
         |
         |}""".stripMargin).methodDeclarations()
  }
}
