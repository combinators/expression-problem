package ep.cpp    /*DD:LD:AI*/

import ep.domain.{Evolution, M0, M1, M2, M3}

/**
  * Truly independent of the specific design solution.
  *
  * Still C++-based, naturally and CPPUnit
  */
trait cpp_e3 extends Evolution with CPPGenerator with TestGenerator with M0 with M1 with M2 with M3 {
  self:cpp_e0 with cpp_e1 with cpp_e2 =>

  import domain._

  abstract override def logic(exp:domain.Atomic, op:domain.Operation): Seq[CPPElement] = {
    // generate the actual body
    op match {
      case PrettyP => {
        exp match {
          case Mult => result(new CPPElement(s""" "(" + ${dispatch(expression(exp, base.left), op)} + "*" + ${dispatch(expression(exp, base.right), op)} + ")" """))
          case Divd => result(new CPPElement(s""" "(" + ${dispatch(expression(exp, base.left), op)} + "/" + ${dispatch(expression(exp, base.right), op)} + ")" """))
          case Neg => result(new CPPElement(s""" "-" + ${dispatch(expression(exp,base.inner), op)} """))

          case _ => super.logic(exp, op)
        }
      }

      case Eval =>
        exp match {
          case Mult => result(new CPPElement(s"${dispatch(expression(exp, base.left), op)} * ${dispatch(expression(exp, base.right), op)}"))
          case Divd => result(new CPPElement(s"${dispatch(expression(exp, base.left), op)} / ${dispatch(expression(exp, base.right), op)}"))
          case Neg => result(new CPPElement(s"- ${dispatch(expression(exp,base.inner), op)} "))

          case _ => super.logic(exp, op)
        }

      case _ => super.logic(exp, op)
    }
  }

  abstract override def testGenerator: Seq[Seq[CPPElement]] = {
    super.testGenerator ++ testMethod(M3_tests)
  }
}
