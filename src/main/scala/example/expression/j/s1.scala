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
trait s1 extends AbstractGenerator with TestGenerator with Producer {
  val domain:ShapeDomain

  /** Eval operation needs to provide specification for current datatypes, namely Lit and Add. */
  abstract override def logic(exp:domain.Atomic)(op:domain.Operation): Seq[Statement] = {
    val subs:Map[String,Expression] = subExpressions(exp)

    // generate the actual body
    op match {
      case domain.Shrink =>
        exp match {
          case domain.Circle =>
            Java(
              s"""
                 |double shrunkRadius = ${subs(domain.attributes.radius)}*pct;
                 return ${inst(domain.Circle)(op)(Java("shrunkRadius").expression[Expression]())};
               """.stripMargin).statements()

          case domain.Square => {
            val str =
              s"""
                 |double shrunkSide = ${subs(domain.attributes.side)}*pct;
                 |return ${inst(domain.Square)(op)(Java("shrunkSide").expression[Expression]())};
               """.stripMargin
            println(str)
            Java(str).statements()
          }

          // return ${recurseOnWithParams(convert(s1, domain.emptyModel()), domain.ContainsPt, Java("t").expression[Expression]())});

          case domain.Translate => {
            Java(
              s"""
                 |return ${inst(domain.Translate)(op)(subs(domain.attributes.trans),recurseOn(subs(domain.attributes.shape), op, Java("pct").expression[Expression]()))};
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
      case ti:domain.TranslateInst => {
        val tuple = ti.i.get.asInstanceOf[((Double,Double),domain.AtomicInst)]
        val pt = s"new java.awt.geom.Point2D.Double(${tuple._1._1}, ${tuple._1._2})"

        Java(s"new $name($pt, ${convert(tuple._2)})").expression()
      }

      case _ => super.convert(inst)
    }
  }

  abstract override def testGenerator: Seq[MethodDeclaration] = {

    val s1 = new domain.SquareInst(8.0)
    val d1 = Java("0.5").expression[Expression]()

    super.testGenerator ++ Java(
      s"""
         |public void test() {
         |   // without access to the attributes, we can't write meaningful attribute test cases.
         |   assertNotNull( ${recurseOn(convert(s1), domain.Shrink, d1)});
         |
         |   // Handle collect checks
         |
         |
         |}""".stripMargin).methodDeclarations()
  }

}

