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
trait e4 extends AbstractGenerator with TestGenerator {
  val domain:Domain
  import domain._

  abstract override def typeGenerator(tpe:types.Types) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case el:List => Java(s"java.util.List<${typeGenerator(el.generic)}>").tpe()
      case _ => super.typeGenerator(tpe)
    }
  }

  abstract override def methodBodyGenerator(exp:expressions.Exp)(op:Operation): Seq[Statement] = {
    val subs = subExpressions(exp)
    val zero = Java("0.0").expression[Expression]()
    val one = Java("1.0").expression[Expression]()
    val negOne = Java("-1.0").expression[Expression]()

    // generate the actual body
    op match {
        // Simplify only works for solutions that instantiate expression instances
      case Simplify =>

        exp match {
          case Lit => Java(s"return ${inst(Lit)(op)(subs(attributes.value))};").statements()
          case Add => Java(s"""|double leftVal = ${recurseOn(subs(base.left), Eval)};
                               |double rightVal = ${recurseOn(subs(base.right), Eval)};
                               |if ((leftVal == 0 && rightVal == 0) || (leftVal + rightVal == 0)) {
                               |  return ${inst(Lit)(op)(zero)};
                               |} else if (leftVal == 0) {
                               |  return ${recurseOn(subs(base.right), Simplify)};
                               |} else if (rightVal == 0) {
                               |  return ${recurseOn(subs(base.left), Simplify)};
                               |} else {
                               |  return ${inst(Add)(op)(recurseOn(subs(base.left), Simplify),recurseOn(subs(base.right), Simplify))};
                               |}""".stripMargin).statements()
          case Sub => Java(s"""
                              |if (${recurseOn(subs(base.left), Eval)} == ${recurseOn(subs(base.right), Eval)}) {
                              |  return ${inst(Lit)(op)(zero)};
                              |} else {
                                return ${inst(Sub)(op)(recurseOn(subs(base.left), Simplify),recurseOn(subs(base.right), Simplify))};
                              |}
                              |""".stripMargin).statements()
          case Mult => Java(s"""
                               |double leftVal = ${recurseOn(subs(base.left), Eval)};
                               |double rightVal = ${recurseOn(subs(base.right), Eval)};
                               |if (leftVal == 0 || rightVal == 0) {
                               |  return ${inst(Lit)(op)(zero)};
                               |} else if (leftVal == 1) {
                               |  return ${recurseOn(subs(base.right), Simplify)};
                               |} else if (rightVal == 1) {
                               |  return ${recurseOn(subs(base.left), Simplify)};
                               |} else {
                                 return ${inst(Mult)(op)(recurseOn(subs(base.left), Simplify),recurseOn(subs(base.right), Simplify))};
                               |}
                               |""".stripMargin).statements()
          case Divd => Java(s"""
                               |double leftVal = ${recurseOn(subs(base.left), Eval)};
                               |double rightVal = ${recurseOn(subs(base.right), Eval)};
                               |if (leftVal == 0) {
                               |  return ${inst(Lit)(op)(zero)};
                               |} else if (rightVal == 1) {
                               |  return ${recurseOn(subs(base.left), Simplify)};
                               |} else if (leftVal == rightVal) {
                               |  return ${inst(Lit)(op)(one)};
                               |} else if (leftVal == -rightVal) {
                               |  return ${inst(Lit)(op)(negOne)};
                               |} else {
                                 return ${inst(Divd)(op)(recurseOn(subs(base.left), Simplify),recurseOn(subs(base.right), Simplify))};
                               |}
                               |""".stripMargin).statements()
          case Neg => Java(s"""
                              |if (${recurseOn(subs(base.exp), Eval)} == 0) {
                              |  return ${inst(Lit)(op)(zero)};
                              |} else {
                              |  return ${inst(Neg)(op)(recurseOn(subs(base.exp), Simplify))};
                              |}""".stripMargin).statements()
          case _ => super.methodBodyGenerator(exp)(op)
        }

      case Collect =>
        exp match {
          case Add | Sub | Mult | Divd => Java(
            s"""|${typeGenerator(List(Double))} list = ${recurseOn(subs(base.left), Collect)};
                |list.addAll(${recurseOn(subs(base.right), Collect)});
                |return list;
                |""".stripMargin).statements()

          case Lit => Java(
              s"""|${typeGenerator(List(Double))} list = new java.util.ArrayList<Double>();
                  |list.add(${subs(attributes.value)});
                  |return list;
                  |""".stripMargin).statements()

          case Neg => Java(s"return ${recurseOn(subs(base.exp), Collect)};").statements()
          case _ => super.methodBodyGenerator(exp)(op)
        }

      case _ => super.methodBodyGenerator(exp)(op)
    }
  }

  abstract override def testGenerator(model:Model): Seq[MethodDeclaration] = {

    // (5/7) / (7-(2*3) --> just (5/7)
    val mult2 = new BinaryInst(Mult, new BinaryInst (Divd, new LitInst(5.0), new LitInst(2.0)), new LitInst(4.0))
    val d1 = new BinaryInst(Divd, new LitInst(5.0), new LitInst(7.0))
    val m1 = new BinaryInst(Mult, new LitInst(2.0), new LitInst(3.0))
    val s1 = new BinaryInst(Sub, new LitInst(7.0), m1)
    val d2 = new BinaryInst(Divd, d1, s1)

    // could split up collect as well.
    super.testGenerator(model.last) ++ {
      val simplifyTests:String  = if (model.supports(Simplify)) {
        s"""
           |assertEquals("((5.0/2.0)*4.0)", ${recurseOn(convert(mult2, model), PrettyP)});
           |assertEquals (${recurseOn(convert(d1, model), PrettyP)}, ${recurseOn(recurseOn(convert(d2, model), Simplify), PrettyP)});
           |
         """.stripMargin
      } else { "" }

      Java(
        s"""
           |public void test() {
           |
           |   $simplifyTests
           |   // Handle collect checks
           |   ${typeGenerator(List(Double))} list1 = ${recurseOn(convert(d2, model), Collect)};
           |   ${typeGenerator(List(Double))} result = new java.util.ArrayList<Double>();
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
