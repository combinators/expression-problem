package example.expression.scala   /*DD:LD:AI*/

import example.expression.domain._
import scala.meta._

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e4 extends Evolution with ScalaGenerator with TestGenerator with OperationDependency with Producer with M0 with M1 with M2 with M3 with M4 {
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

        continue(Scala("Seq(" + seq.mkString(",") + ")").expression())

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
      case el:List => Scala(s"Seq[${typeConverter(el.generic)}]").tpe()
      case _ => super.typeConverter(tpe)
    }
  }

  abstract override def logic(exp:Atomic)(op:Operation): Seq[Statement] = {
    val subs:Map[String, Term] = subExpressions(exp).asInstanceOf[Map[String,Term]]
    val zero = Scala("0.0").expression()
    val one = Scala("1.0").expression()
    val negOne = Scala("-1.0").expression()

    // generate the actual body
    op match {
        // Simplify only works for solutions that instantiate expression instances
      case Simplify =>

        exp match {
          case Lit => Scala(s" ${inst(Lit)(op)(subs(litValue))}").statements()
          case Add => Scala(s"""
                               |val leftVal = ${dependentDispatch(subs(domain.base.left), Eval)}
                               |val rightVal = ${dependentDispatch(subs(domain.base.right), Eval)}
                               |if ((leftVal == 0 && rightVal == 0) || (leftVal + rightVal == 0)) {
                               |  ${inst(Lit)(op)(zero)}
                               |} else if (leftVal == 0) {
                               |  ${dispatch(subs(domain.base.right), Simplify)}
                               |} else if (rightVal == 0) {
                               |  ${dispatch(subs(domain.base.left), Simplify)}
                               |} else {
                               |  ${inst(Add)(op)(dispatch(subs(domain.base.left), Simplify),dispatch(subs(domain.base.right), Simplify))}
                               |}""".stripMargin).statements()
          case Sub => Scala(s"""
                              |if (${dependentDispatch(subs(domain.base.left), Eval)} == ${dependentDispatch(subs(domain.base.right), Eval)}) {
                              |  ${inst(Lit)(op)(zero)}
                              |} else {
                              |  ${inst(Sub)(op)(dispatch(subs(domain.base.left), Simplify),dispatch(subs(domain.base.right), Simplify))}
                              |}
                              |""".stripMargin).statements()
          case Mult => Scala(s"""
                               |val leftVal = ${dependentDispatch(subs(domain.base.left), Eval)}
                               |val rightVal = ${dependentDispatch(subs(domain.base.right), Eval)}
                               |if (leftVal == 0 || rightVal == 0) {
                               |  ${inst(Lit)(op)(zero)}
                               |} else if (leftVal == 1) {
                               |  ${dispatch(subs(domain.base.right), Simplify)}
                               |} else if (rightVal == 1) {
                               |  ${dispatch(subs(domain.base.left), Simplify)}
                               |} else {
                               |   ${inst(Mult)(op)(dispatch(subs(domain.base.left), Simplify),dispatch(subs(domain.base.right), Simplify))}
                               |}
                               |""".stripMargin).statements()
          case Divd => Scala(s"""
                               |val leftVal = ${dependentDispatch(subs(domain.base.left), Eval)}
                               |val rightVal = ${dependentDispatch(subs(domain.base.right), Eval)}
                               |if (leftVal == 0) {
                               |   ${inst(Lit)(op)(zero)}
                               |} else if (rightVal == 1) {
                               |   ${dispatch(subs(domain.base.left), Simplify)}
                               |} else if (leftVal == rightVal) {
                               |   ${inst(Lit)(op)(one)}
                               |} else if (leftVal == -rightVal) {
                               |   ${inst(Lit)(op)(negOne)}
                               |} else {
                               |   ${inst(Divd)(op)(dispatch(subs(domain.base.left), Simplify),dispatch(subs(domain.base.right), Simplify))}
                               |}
                               |""".stripMargin).statements()
            // TODO: Would love to have ability to simplify neg(neg(x)) to just be x. This requires a form
            // of inspection that might not be generalizable...
          case Neg => Scala(s"""
                              |if (${dependentDispatch(subs(domain.base.inner), Eval)} == 0) {
                              |   ${inst(Lit)(op)(zero)}
                              |} else {
                              |   ${inst(Neg)(op)(dispatch(subs(domain.base.inner), Simplify))}
                              |}""".stripMargin).statements()
          case _ => super.logic(exp)(op)
        }

      case Collect =>
        exp match {

          case _:domain.Binary => Scala(s"${dispatch(subs(domain.base.left), Collect)} ++ ${dispatch(subs(domain.base.right), Collect)}").statements()
          case _:domain.Unary  => Scala(s"${dispatch(subs(domain.base.inner), Collect)}").statements()
          case at:domain.Atomic => {
            at match {
              case Lit => Scala(s"Seq(${subs(litValue).toString})").statements()
              case _  => Scala(s"${dispatch(subs(litValue), Collect)}").statements()
            }
          }

          case _ => super.logic(exp)(op)
        }

      case _ => super.logic(exp)(op)
    }
  }

  // TODO: HACK. Fix this implementation
  abstract override def testGenerator: Seq[Stat] = {
    super.testGenerator :+ testMethod(M4_tests)
//    val d1 = new domain.BinaryInst(Divd, new LitInst(5.0), new LitInst(7.0))
//      Scala(
//        s"""def test() : Unit = {
//           |  assert ("((5.0/2.0)*4.0)" == ${dispatch(convert(m4_m1), PrettyP)});
//           |  assert (${dispatch(convert(d1), PrettyP)} == ${dispatch(dispatch(convert(m4_d2), Simplify), PrettyP)});
//           |}
//         """.stripMargin).statements() ++ super.testGenerator :+ testMethod(M4_tests)
  }
}
