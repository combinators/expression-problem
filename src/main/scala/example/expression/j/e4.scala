package example.expression.j

import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.expr.Expression
import com.github.javaparser.ast.stmt.Statement
import example.expression.domain.Domain
import org.combinators.templating.twirl.Java

/**
  * Truly independent of the specific design solution.
  *
  * Still Java-based, naturally and JUnit
  */
trait e4 extends AbstractGenerator with TestGeneratorWithModel {
  val domain:Domain

  def getModel:domain.Model

  abstract override def typeGenerator(tpe:domain.Types) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case el:domain.List => Java(s"java.util.List<${typeGenerator(el.generic)}>").tpe()
      case _ => super.typeGenerator(tpe)
    }
  }

  abstract override def logic(exp:domain.subtypes.Exp)(op:domain.Operation): Seq[Statement] = {
    val subs = subExpressions(exp)
    val zero = Java("0.0").expression[Expression]()
    val one = Java("1.0").expression[Expression]()
    val negOne = Java("-1.0").expression[Expression]()

    // generate the actual body
    op match {
        // Simplify only works for solutions that instantiate expression instances
      case domain.Simplify =>

        exp match {
          case domain.Lit => Java(s"return ${inst(domain.Lit)(op)(subs(domain.attributes.value))};").statements()
          case domain.Add => Java(s"""|double leftVal = ${recurseOn(subs(domain.base.left), domain.Eval)};
                               |double rightVal = ${recurseOn(subs(domain.base.right), domain.Eval)};
                               |if ((leftVal == 0 && rightVal == 0) || (leftVal + rightVal == 0)) {
                               |  return ${inst(domain.Lit)(op)(zero)};
                               |} else if (leftVal == 0) {
                               |  return ${recurseOn(subs(domain.base.right), domain.Simplify)};
                               |} else if (rightVal == 0) {
                               |  return ${recurseOn(subs(domain.base.left), domain.Simplify)};
                               |} else {
                               |  return ${inst(domain.Add)(op)(recurseOn(subs(domain.base.left), domain.Simplify),recurseOn(subs(domain.base.right), domain.Simplify))};
                               |}""".stripMargin).statements()
          case domain.Sub => Java(s"""
                              |if (${recurseOn(subs(domain.base.left), domain.Eval)} == ${recurseOn(subs(domain.base.right), domain.Eval)}) {
                              |  return ${inst(domain.Lit)(op)(zero)};
                              |} else {
                                return ${inst(domain.Sub)(op)(recurseOn(subs(domain.base.left), domain.Simplify),recurseOn(subs(domain.base.right), domain.Simplify))};
                              |}
                              |""".stripMargin).statements()
          case domain.Mult => Java(s"""
                               |double leftVal = ${recurseOn(subs(domain.base.left), domain.Eval)};
                               |double rightVal = ${recurseOn(subs(domain.base.right), domain.Eval)};
                               |if (leftVal == 0 || rightVal == 0) {
                               |  return ${inst(domain.Lit)(op)(zero)};
                               |} else if (leftVal == 1) {
                               |  return ${recurseOn(subs(domain.base.right), domain.Simplify)};
                               |} else if (rightVal == 1) {
                               |  return ${recurseOn(subs(domain.base.left), domain.Simplify)};
                               |} else {
                                 return ${inst(domain.Mult)(op)(recurseOn(subs(domain.base.left), domain.Simplify),recurseOn(subs(domain.base.right), domain.Simplify))};
                               |}
                               |""".stripMargin).statements()
          case domain.Divd => Java(s"""
                               |double leftVal = ${recurseOn(subs(domain.base.left), domain.Eval)};
                               |double rightVal = ${recurseOn(subs(domain.base.right), domain.Eval)};
                               |if (leftVal == 0) {
                               |  return ${inst(domain.Lit)(op)(zero)};
                               |} else if (rightVal == 1) {
                               |  return ${recurseOn(subs(domain.base.left), domain.Simplify)};
                               |} else if (leftVal == rightVal) {
                               |  return ${inst(domain.Lit)(op)(one)};
                               |} else if (leftVal == -rightVal) {
                               |  return ${inst(domain.Lit)(op)(negOne)};
                               |} else {
                                 return ${inst(domain.Divd)(op)(recurseOn(subs(domain.base.left), domain.Simplify),recurseOn(subs(domain.base.right), domain.Simplify))};
                               |}
                               |""".stripMargin).statements()
          case domain.Neg => Java(s"""
                              |if (${recurseOn(subs(domain.base.exp), domain.Eval)} == 0) {
                              |  return ${inst(domain.Lit)(op)(zero)};
                              |} else {
                              |  return ${inst(domain.Neg)(op)(recurseOn(subs(domain.base.exp), domain.Simplify))};
                              |}""".stripMargin).statements()
          case _ => super.logic(exp)(op)
        }

      case domain.Collect =>
        exp match {
          case _:domain.subtypes.BinaryExp => Java(
            s"""|${typeGenerator(domain.List(domain.Double))} list = ${recurseOn(subs(domain.base.left), domain.Collect)};
                |list.addAll(${recurseOn(subs(domain.base.right), domain.Collect)});
                |return list;
                |""".stripMargin).statements()

          case _:domain.subtypes.UnaryExp  => Java(
            s"""|${typeGenerator(domain.List(domain.Double))} list = new java.util.ArrayList<Double>();
                |list.addAll(${recurseOn(subs(domain.base.exp), domain.Collect)});
                |return list;
                |""".stripMargin).statements()

          case _:domain.subtypes.Exp => Java(
            s"""|${typeGenerator(domain.List(domain.Double))} list = new java.util.ArrayList<Double>();
                |list.add(${subs(domain.attributes.value)});
                |return list;
                |""".stripMargin).statements()

          case _ => super.logic(exp)(op)
        }

      case _ => super.logic(exp)(op)
    }
  }

  abstract override def testGenerator: Seq[MethodDeclaration] = {

    // (5/7) / (7-(2*3) --> just (5/7)
    val mult2 = new domain.BinaryInst(domain.Mult, new domain.BinaryInst (domain.Divd, new domain.LitInst(5.0), new domain.LitInst(2.0)), new domain.LitInst(4.0))
    val d1 = new domain.BinaryInst(domain.Divd, new domain.LitInst(5.0), new domain.LitInst(7.0))
    val m1 = new domain.BinaryInst(domain.Mult, new domain.LitInst(2.0), new domain.LitInst(3.0))
    val s1 = new domain.BinaryInst(domain.Sub, new domain.LitInst(7.0), m1)
    val d2 = new domain.BinaryInst(domain.Divd, d1, s1)

    // could split up collect as well.
    super.testGenerator ++ {
      val simplifyTests:String  = if (getModel.supports(domain.Simplify)) {
        s"""
           |assertEquals("((5.0/2.0)*4.0)", ${recurseOn(convert(mult2), domain.PrettyP)});
           |assertEquals (${recurseOn(convert(d1), domain.PrettyP)}, ${recurseOn(recurseOn(convert(d2), domain.Simplify), domain.PrettyP)});
           |
         """.stripMargin
      } else { "" }

      Java(
        s"""
           |public void test() {
           |
           |   $simplifyTests
           |   // Handle collect checks
           |   ${typeGenerator(domain.List(domain.Double))} list1 = ${recurseOn(convert(d2), domain.Collect)};
           |   ${typeGenerator(domain.List(domain.Double))} result = new java.util.ArrayList<Double>();
           |   result.add(5.0);
           |   result.add(7.0);
           |   result.add(7.0);
           |   result.add(2.0);
           |   result.add(3.0);
           |   assertEquals (list1, result);
           |}""".stripMargin).methodDeclarations()
    }
  }
}
