package example.expression.haskell      /*DD:LD:AI*/

import example.expression.domain._

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e3 extends Evolution with HaskellGenerator with HUnitTestGenerator with M0 with M1 with M2 with M3 {
  self:e0 with e1 with e2 =>
  val domain:MathDomain
  import domain._

   abstract override def logic(exp:domain.Atomic, op:domain.Operation): Seq[Haskell] = {
    val atts = subExpressions(exp)
    
    // generate the actual body
    op match {
      case PrettyP => {
        exp match {
          case Neg => result(Haskell(s""" "-" ++ ${dispatch(atts(base.inner), op)} """))
          case Mult => result(Haskell(s""" "(" ++ ${dispatch(atts(base.left), op)} ++ "*" ++ ${dispatch(atts(base.right), op)} ++ ")" """))
          case Divd => result(Haskell(s""" "(" ++ ${dispatch(atts(base.left), op)} ++ "/" ++ ${dispatch(atts(base.right), op)} ++ ")" """))
          case _ => super.logic(exp, op)
        }
      }

      case Eval => {
        exp match {
          case Neg => result(new Haskell(s"(- ${dispatch(atts(base.inner), op)})"))
          case Mult => result(new Haskell(s"${dispatch(atts(base.left), op)} * ${dispatch(atts(base.right), op)}"))
          case Divd => result(new Haskell(s"${dispatch(atts(base.left), op)} / ${dispatch(atts(base.right), op)}"))
          case _ => super.logic(exp, op)
        }
      }
      case _ => super.logic(exp, op)
    }
  }

  abstract override def testGenerator: Seq[Haskell] = {
    super.testGenerator :+ hunitMethod(M3_tests)
  }
}

