package example.expression.j  /*DD:LD:AI*/

import com.github.javaparser.ast.`type`.Type
import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.expr.Expression
import com.github.javaparser.ast.stmt.Statement
import example.expression.domain._
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e4 extends Evolution with AbstractGenerator with TestGenerator with Producer with M0 with M1 with M2 with M3 with M4 {
  self:e0 with e1 with e2 with e3 =>
  val domain:MathDomain

  abstract override def typeConverter(tpe:domain.TypeRep, covariantReplacement:Option[Type] = None) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case el:List => Java(s"java.util.List<${typeConverter(el.generic)}>").tpe()
      case _ => super.typeConverter(tpe, covariantReplacement)
    }
  }

  abstract override def logic(exp:domain.Atomic)(op:domain.Operation): Seq[Statement] = {
    val subs = subExpressions(exp)
    val zero = Java("0.0").expression[Expression]()
    val one = Java("1.0").expression[Expression]()
    val negOne = Java("-1.0").expression[Expression]()

    // generate the actual body
    op match {
        // Simplify only works for solutions that instantiate expression instances
      case Simplify =>

        exp match {
          case Lit => Java(s"return ${inst(Lit)(op)(subs(litValue))};").statements()
          case Add => Java(s"""|double leftVal = ${dependentDispatch(subs(domain.base.left), Eval)};
                               |double rightVal = ${dependentDispatch(subs(domain.base.right), Eval)};
                               |if ((leftVal == 0 && rightVal == 0) || (leftVal + rightVal == 0)) {
                               |  return ${inst(Lit)(op)(zero)};
                               |} else if (leftVal == 0) {
                               |  return ${dispatch(subs(domain.base.right), Simplify)};
                               |} else if (rightVal == 0) {
                               |  return ${dispatch(subs(domain.base.left), Simplify)};
                               |} else {
                               |  return ${inst(Add)(op)(dispatch(subs(domain.base.left), Simplify),dispatch(subs(domain.base.right), Simplify))};
                               |}""".stripMargin).statements()
          case Sub => Java(s"""
                              |if (${dependentDispatch(subs(domain.base.left), Eval)} == ${dependentDispatch(subs(domain.base.right), Eval)}) {
                              |  return ${inst(Lit)(op)(zero)};
                              |} else {
                                return ${inst(Sub)(op)(dispatch(subs(domain.base.left), Simplify),dispatch(subs(domain.base.right), Simplify))};
                              |}
                              |""".stripMargin).statements()
          case Mult => Java(s"""
                               |double leftVal = ${dependentDispatch(subs(domain.base.left), Eval)};
                               |double rightVal = ${dependentDispatch(subs(domain.base.right), Eval)};
                               |if (leftVal == 0 || rightVal == 0) {
                               |  return ${inst(Lit)(op)(zero)};
                               |} else if (leftVal == 1) {
                               |  return ${dispatch(subs(domain.base.right), Simplify)};
                               |} else if (rightVal == 1) {
                               |  return ${dispatch(subs(domain.base.left), Simplify)};
                               |} else {
                                 return ${inst(Mult)(op)(dispatch(subs(domain.base.left), Simplify),dispatch(subs(domain.base.right), Simplify))};
                               |}
                               |""".stripMargin).statements()
          case Divd => Java(s"""
                               |double leftVal = ${dependentDispatch(subs(domain.base.left), Eval)};
                               |double rightVal = ${dependentDispatch(subs(domain.base.right), Eval)};
                               |if (leftVal == 0) {
                               |  return ${inst(Lit)(op)(zero)};
                               |} else if (rightVal == 1) {
                               |  return ${dispatch(subs(domain.base.left), Simplify)};
                               |} else if (leftVal == rightVal) {
                               |  return ${inst(Lit)(op)(one)};
                               |} else if (leftVal == -rightVal) {
                               |  return ${inst(Lit)(op)(negOne)};
                               |} else {
                                 return ${inst(Divd)(op)(dispatch(subs(domain.base.left), Simplify),dispatch(subs(domain.base.right), Simplify))};
                               |}
                               |""".stripMargin).statements()
            // TODO: Would love to have ability to simplify neg(neg(x)) to just be x. This requires a form
            // of inspection that might not be generalizable...
          case Neg => Java(s"""
                              |if (${dependentDispatch(subs(domain.base.inner), Eval)} == 0) {
                              |  return ${inst(Lit)(op)(zero)};
                              |} else {
                              |  return ${inst(Neg)(op)(dispatch(subs(domain.base.inner), Simplify))};
                              |}""".stripMargin).statements()
          case _ => super.logic(exp)(op)
        }

      case Collect =>
        exp match {
          case _:domain.Binary => Java(
            s"""|${typeConverter(List(Double))} list = ${dispatch(subs(domain.base.left), Collect)};
                |list.addAll(${dispatch(subs(domain.base.right), Collect)});
                |return list;
                |""".stripMargin).statements()

          case _:domain.Unary  => Java(
            s"""|${typeConverter(List(Double))} list = new java.util.ArrayList<Double>();
                |list.addAll(${dispatch(subs(domain.base.inner), Collect)});
                |return list;
                |""".stripMargin).statements()

          case _:domain.Atomic => Java(
            s"""|${typeConverter(List(Double))} list = new java.util.ArrayList<Double>();
                |list.add(${subs(litValue)});
                |return list;
                |""".stripMargin).statements()

          case _ => super.logic(exp)(op)
        }

      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testGenerator: Seq[MethodDeclaration] = {

    // (5/7) / (7-(2*3) --> just (5/7)
    val mult2 = new domain.BinaryInst(Mult, new domain.BinaryInst (Divd, new LitInst(5.0), new LitInst(2.0)), new LitInst(4.0))
    val d1 = new domain.BinaryInst(Divd, new LitInst(5.0), new LitInst(7.0))
    val m1 = new domain.BinaryInst(Mult, new LitInst(2.0), new LitInst(3.0))
    val s1 = new domain.BinaryInst(Sub, new LitInst(7.0), m1)
    val d2 = new domain.BinaryInst(Divd, d1, s1)

    // could split up collect as well.
    super.testGenerator ++ {
      val simplifyTests:String  = if (getModel.supports(Simplify)) {
        s"""
           |assertEquals("((5.0/2.0)*4.0)", ${dispatch(convert(mult2), PrettyP)});
           |assertEquals (${dispatch(convert(d1), PrettyP)}, ${dispatch(dispatch(convert(d2), Simplify), PrettyP)});
           |
         """.stripMargin
      } else { "" }

      Java(
        s"""
           |public void test() {
           |
           |   $simplifyTests
           |   // Handle collect checks
           |   ${typeConverter(List(Double))} list1 = ${dispatch(convert(d2), Collect)};
           |   ${typeConverter(List(Double))} result = new java.util.ArrayList<Double>();
           |   result.add(5.0);
           |   result.add(7.0);
           |   result.add(7.0);
           |   result.add(2.0);
           |   result.add(3.0);
           |   assertEquals (list1, result);
           |}""".stripMargin).methodDeclarations
    }
  }
}
