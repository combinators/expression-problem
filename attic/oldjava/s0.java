package org.combinators.ep.language.java;

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait s0 extends Evolution with DomainIndependentJavaGenerator with JUnitTestGenerator with S0 {
  val domain:ShapeDomain

  /** E0 Introduces the concept a Double type, used for the 'Eval' operation. */
  abstract override def typeConverter(tpe:domain.TypeRep) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case Double  => Java("Double").tpe()
      case Point2D => Java("java.awt.geom.Point2D.Double").tpe()
      case Boolean => Java("Boolean").tpe()
      case _ => super.typeConverter(tpe)
    }
  }

  /** E0 Introduces Double and Int values. */
  abstract override def toTargetLanguage(ei:domain.ExistsInstance) : CodeBlockWithResultingExpressions = {
    ei.inst match {
      case d:scala.Double => CodeBlockWithResultingExpressions(Java(s"$d").expression())
      case b:scala.Boolean => CodeBlockWithResultingExpressions(Java(s"$b").expression())
      case (x: Double, y: Double) =>
        CodeBlockWithResultingExpressions(
          Java(s"new java.awt.geom.Point2D.Double($x, $y)").expression()
        )
      case _ => super.toTargetLanguage(ei)
    }
  }

  /** Convert a test instance into a Java Expression for instantiating that instance. */
  abstract override def toTargetLanguage(instance:domain.Inst) : CodeBlockWithResultingExpressions = {
    instance match {
      case ti:TranslateInst =>
        toTargetLanguage(ti.s).appendDependent { case Seq(innerExp) =>
          toTargetLanguage(ti.ei).appendDependent { case Seq(offsetExp) =>
            inst(ti.e, offsetExp, innerExp)
          }
        }
      case _ => super.toTargetLanguage(instance)
    }
  }

  /** Eval operation needs to provide specification for current datatypes, namely Lit and Add. */
  abstract override def logic(exp:domain.DataType, op:domain.Operation): Seq[Statement] = {
    op match {
      case ContainsPt =>
        exp match {
          case Circle =>
            result(Java(s" Math.sqrt(point.x*point.x + point.y*point.y) <= ${expression(exp, radius)}").expression())

          case Square =>
            result(Java(s" (Math.abs(point.x) <= ${expression(exp, side)}/2 && Math.abs(point.y) <= ${expression(exp, side)}/2)").expression())

          case Translate =>
            val str = s"""
                   |// first adjust
                   |java.awt.geom.Point2D.Double t = new java.awt.geom.Point2D.Double(point.x - ${expression(exp, trans)}.x, point.y - ${expression(exp, trans)}.y);""".stripMargin
            val res = result(dispatch(expression(exp, shape), ContainsPt, Java("t").expression()))
            Java(str).statements() ++ res
        }

      case _ => super.logic(exp, op)
    }
  }



  override def junitTestMethod(test:domain.TestCase, idx:Int) : Seq[Statement] = {
      test match {
        case ctc: ContainsTestCase =>
          val pointBlock = toTargetLanguage(ctc.pti)
          val actualBlock = pointBlock.appendDependent { case Seq(pt) =>
            actual(ContainsPt, ctc.inst, pt)
          }

          actualBlock.appendDependent { case Seq(actual) =>
            CodeBlockWithResultingExpressions(
              if (ctc.result) {
                Java(s"assertTrue($actual);").statement()
              } else {
                Java(s"assertFalse($actual);").statement()
              }
            )()
          }.block
        case _ => super.junitTestMethod(test, idx)
    }
  }

  abstract override def testGenerator: Seq[MethodDeclaration] = {
    super.testGenerator ++ testMethod(S0_tests)
  }
}
