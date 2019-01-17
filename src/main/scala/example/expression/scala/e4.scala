package example.expression.scala   /*DD:LD:AI*/

import example.expression.domain._
import scala.meta._

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e4 extends Evolution with ScalaGenerator with TestGenerator with OperationDependency with M0 with M1 with M2 with M3 with M4 {
  self:e0 with e1 with e2 with e3 =>
  val domain:MathDomain
  import domain._

  /**
    * List can be accommodated (in Java) by populating ArrayList with values drawn from test case.
    */
   override def expected(test:domain.TestCaseExpectedValue, id:String) : (Expression => Stat) => Stat = continue => {
    test.expect._1 match {  // was op.returnType.get
      case list:List =>
        val seq: Seq[Any] = test.expect._2.asInstanceOf[Seq[Any]]

        continue(Scala("Seq(" + seq.mkString(",") + ")").expression)

      case _ => super.expected(test,id)(continue)
    }
  }

  /**
    * Operations can declare dependencies, which leads to #include extras
    */
  override def dependency(op: domain.Operation): scala.List[domain.Operation] = {
    op match {
      case Simplify => scala.List[domain.Operation](PrettyP, Eval)
      case _ => super.dependency(op)
    }
  }

  abstract override def typeConverter(tpe:domain.TypeRep) : Type = {
    tpe match {
      case el:List => Scala(s"Seq[${typeConverter(el.generic)}]").tpe
      case _ => super.typeConverter(tpe)
    }
  }

  abstract override def logic(exp:Atomic, op:Operation): Seq[Statement] = {
    val subs:Map[String, Term] = subExpressions(exp).asInstanceOf[Map[String,Term]]
    val zero = Scala("0.0").expression
    val one = Scala("1.0").expression
    val negOne = Scala("-1.0").expression

    // generate the actual body
    op match {
        // Simplify only works for solutions that instantiate expression instances
      case Simplify =>

        exp match {
          case Lit => Scala(s" ${inst(Lit, subs(litValue))}").statements
          case Add => Scala(s"""
                               |val leftVal = ${dependentDispatch(subs(domain.base.left), Eval)}
                               |val rightVal = ${dependentDispatch(subs(domain.base.right), Eval)}
                               |if ((leftVal == 0 && rightVal == 0) || (leftVal + rightVal == 0)) {
                               |  ${result(inst(Lit, zero)).mkString("\n")}
                               |} else if (leftVal == 0) {
                               |  ${result(dispatch(subs(domain.base.right), Simplify)).mkString("\n")}
                               |} else if (rightVal == 0) {
                               |  ${result(dispatch(subs(domain.base.left), Simplify)).mkString("\n")}
                               |} else {
                               |  ${result(inst(Add, dispatch(subs(domain.base.left), Simplify),dispatch(subs(domain.base.right), Simplify))).mkString("\n")}
                               |}""".stripMargin).statements
          case Sub => Scala(s"""
                              |if (${dependentDispatch(subs(domain.base.left), Eval)} == ${dependentDispatch(subs(domain.base.right), Eval)}) {
                              |  ${result(inst(Lit, zero)).mkString("\n")}
                              |} else {
                              |  ${result(inst(Sub, dispatch(subs(domain.base.left), Simplify),dispatch(subs(domain.base.right), Simplify))).mkString("\n")}
                              |}
                              |""".stripMargin).statements
          case Mult => Scala(s"""
                               |val leftVal = ${dependentDispatch(subs(domain.base.left), Eval)}
                               |val rightVal = ${dependentDispatch(subs(domain.base.right), Eval)}
                               |if (leftVal == 0 || rightVal == 0) {
                               |  ${result(inst(Lit, zero)).mkString("\n")}
                               |} else if (leftVal == 1) {
                               |  ${result(dispatch(subs(domain.base.right), Simplify)).mkString("\n")}
                               |} else if (rightVal == 1) {
                               |  ${result(dispatch(subs(domain.base.left), Simplify)).mkString("\n")}
                               |} else {
                               |   ${result(inst(Mult, dispatch(subs(domain.base.left), Simplify),dispatch(subs(domain.base.right), Simplify))).mkString("\n")}
                               |}
                               |""".stripMargin).statements
          case Divd => Scala(s"""
                               |val leftVal = ${dependentDispatch(subs(domain.base.left), Eval)}
                               |val rightVal = ${dependentDispatch(subs(domain.base.right), Eval)}
                               |if (leftVal == 0) {
                               |   ${result(inst(Lit, zero)).mkString("\n")}
                               |} else if (rightVal == 1) {
                               |   ${result(dispatch(subs(domain.base.left), Simplify)).mkString("\n")}
                               |} else if (leftVal == rightVal) {
                               |   ${result(inst(Lit, one)).mkString("\n")}
                               |} else if (leftVal == -rightVal) {
                               |   ${result(inst(Lit, negOne)).mkString("\n")}
                               |} else {
                               |   ${result(inst(Divd, dispatch(subs(domain.base.left), Simplify),dispatch(subs(domain.base.right), Simplify))).mkString("\n")}
                               |}
                               |""".stripMargin).statements
            // TODO: Would love to have ability to simplify neg(neg(x)) to just be x. This requires a form
            // of inspection that might not be generalizable...
          case Neg => Scala(s"""
                              |if (${dependentDispatch(subs(domain.base.inner), Eval)} == 0) {
                              |   ${result(inst(Lit, zero)).mkString("\n")}
                              |} else {
                              |   ${result(inst(Neg, dispatch(subs(domain.base.inner), Simplify))).mkString("\n")}
                              |}""".stripMargin).statements
          case _ => super.logic(exp, op)
        }

      case Collect =>
        exp match {

          case _:domain.Binary => result(Scala(s"${dispatch(subs(domain.base.left), Collect)} ++ ${dispatch(subs(domain.base.right), Collect)}").expression)
          case _:domain.Unary  => result(Scala(s"${dispatch(subs(domain.base.inner), Collect)}").expression)
          case at:domain.Atomic => {
            at match {
              case Lit => result(Scala(s"Seq(${subs(litValue).toString})").expression)
              case _  => result(Scala(s"${dispatch(subs(litValue), Collect)}").expression)
            }
          }

          case _ => super.logic(exp, op)
        }

      case _ => super.logic(exp, op)
    }
  }

  abstract override def testGenerator: Seq[Stat] = {
    super.testGenerator :+ testMethod(M4_tests)
  }
}
