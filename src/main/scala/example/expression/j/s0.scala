package example.expression.j  /*DD:LD:AI*/

import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.stmt.BlockStmt
import example.expression.domain.{Evolution, S0, ShapeDomain}
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait s0 extends Evolution with JavaGenerator with JUnitTestGenerator with S0 {
  val domain:ShapeDomain

  /** E0 Introduces the concept a Double type, used for the 'Eval' operation. */
  abstract override def typeConverter(tpe:domain.TypeRep) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case Double => Java("Double").tpe()
      case Point2D => Java("java.awt.geom.Point2D.Double").tpe()
      case Boolean => Java("Boolean").tpe()
      case _ => super.typeConverter(tpe)
    }
  }

  /** Eval operation needs to provide specification for current datatypes, namely Lit and Add. */
  abstract override def logic(exp:domain.Atomic, op:domain.Operation): Seq[Statement] = {
    val subs:Map[String,Expression] = subExpressions(exp).asInstanceOf[Map[String,Expression]]

    // generate the actual body
    op match {
      case ContainsPt =>
        exp match {
          case Circle =>
            result(Java(s" Math.sqrt(point.x*point.x + point.y*point.y) <= ${subs(radius)}").expression[Expression]())

          case Square =>
            result(Java(s" (Math.abs(point.x) <= ${subs(side)}/2 && Math.abs(point.y) <= ${subs(side)}/2)").expression[Expression]())

          case Translate =>
            Java(
              s"""
                 |// first adjust
                 |java.awt.geom.Point2D.Double t = new java.awt.geom.Point2D.Double(point.x - ${subs(trans)}.x, point.y - ${subs(trans)}.y);
                 |${result(dispatch(subs(shape), ContainsPt, Java("t").expression[Expression]()))}""".stripMargin).statements()

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

  override def junitTestMethod(test:domain.TestCase, idx:Int) : Seq[Statement] = {
      test match {
        case ctc: ContainsTestCase =>
          val x = Java(ctc.pt._1.toString)
          val y = Java(ctc.pt._2.toString)
          val pt = Java(s"new java.awt.geom.Point2D.Double ($x,$y)").expression[Expression]()

          if (ctc.result) {
            Java(s"assertTrue(${actual(ContainsPt, ctc.inst, pt)});").statements
          } else {
            Java(s"assertFalse(${actual(ContainsPt, ctc.inst, pt)});").statements
          }
        case _ => super.junitTestMethod(test, idx)
    }
  }

  abstract override def testGenerator: Seq[MethodDeclaration] = {
    super.testGenerator ++ testMethod(S0_tests)
  }
}
