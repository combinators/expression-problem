package example.expression.j

import com.github.javaparser.ast.body.MethodDeclaration
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
    // generate the actual body
    op match {
      case Simplify =>
        exp match {
          case Lit => Java (s"return new Lit(${subs(0)});").statements()
          case Add => Java(s"""|double leftVal = ${recurseOn(subs(0), Eval)};
                               |double rightVal = ${recurseOn(subs(1), Eval)};
                               |if ((leftVal == 0 && rightVal == 0) || (leftVal + rightVal == 0)) {
                               |  return new Lit(0.0);
                               |} else if (leftVal == 0) {
                               |  return ${recurseOn(subs(1), Simplify)};
                               |} else if (rightVal == 0) {
                               |  return ${recurseOn(subs(0), Simplify)};
                               |} else {
                               |  return new Add(${recurseOn(subs(0), Simplify)}, ${recurseOn(subs(1), Simplify)});
                               |}""".stripMargin).statements()
          case Sub => Java(s"""
                              |if (${recurseOn(subs(0), Eval)} == ${recurseOn(subs(1), Eval)}) {
                              |  return new Lit(0.0);
                              |} else {
                              |  return new Sub(${recurseOn(subs(0), Simplify)}, ${recurseOn(subs(1), Simplify)});
                              |}
                              |""".stripMargin).statements()
          case Mult => Java(s"""
                               |double leftVal = ${recurseOn(subs(0), Eval)};
                               |double rightVal = ${recurseOn(subs(1), Eval)};
                               |if (leftVal == 0 || rightVal == 0) {
                               |  return new Lit(0.0);
                               |} else if (leftVal == 1) {
                               |  return ${recurseOn(subs(1), Simplify)};
                               |} else if (rightVal == 1) {
                               |  return ${recurseOn(subs(0), Simplify)};
                               |} else {
                               |  return new Mult(${recurseOn(subs(0), Simplify)}, ${recurseOn(subs(1), Simplify)});
                               |}
                               |""".stripMargin).statements()
          case Divd => Java(s"""
                               |double leftVal = ${recurseOn(subs(0), Eval)};
                               |double rightVal = ${recurseOn(subs(1), Eval)};
                               |if (leftVal == 0) {
                               |  return new Lit(0.0);
                               |} else if (rightVal == 1) {
                               |  return ${recurseOn(subs(0), Simplify)};
                               |} else if (leftVal == rightVal) {
                               |  return new Lit(1.0);
                               |} else if (leftVal == -rightVal) {
                               |  return new Lit(-1.0);
                               |} else {
                               |  return new Divd(${recurseOn(subs(0), Simplify)}, ${recurseOn(subs(1), Simplify)});
                               |}
                               |""".stripMargin).statements()
          case Neg => Java(s"""
                              |if (${recurseOn(subs(0), Eval)} == 0) {
                              |  return new Lit(0.0);
                              |} else {
                              |  return new Neg(${recurseOn(subs(0), Simplify)});
                              |}""".stripMargin).statements()
          case _ => super.methodBodyGenerator(exp)(op)
        }

      case Collect =>
        exp match {
          case Add | Sub | Mult | Divd => Java(
            s"""|java.util.List<Double> list = ${recurseOn(subs(0), Collect)};
                |list.addAll(${recurseOn(subs(1), Collect)});
                |return list;
                |""".stripMargin).statements()

          case Lit => Java(s"return java.util.Collections.singletonList(${subs(0)});").statements()
          case Neg => Java(s"return ${recurseOn(subs(0), Collect)};").statements()
          case _ => super.methodBodyGenerator(exp)(op)
        }

      case _ => super.methodBodyGenerator(exp)(op)
    }
  }

  abstract override def testGenerator(): Seq[MethodDeclaration] = {

    // (5/7) / (7-(2*3) --> just (5/7)
    val d1 = new BinaryInst(Divd, new LitInst(5.0), new LitInst(7.0))
    val m1 = new BinaryInst(Mult, new LitInst(2.0), new LitInst(3.0))
    val s1 = new BinaryInst(Sub, new LitInst(7.0), m1)
    val d2 = new BinaryInst(Divd, d1, s1)

    super.testGenerator() ++ Java(
      s"""
         |public void test() {
         |   Exp exp1 = new Neg(new Lit(1.0));
         |   assertEquals("-1.0", ${recurseOn(Java("exp1").expression(), PrettyP)});
         |   assertEquals(-1.0, ${recurseOn(Java("exp1").expression(), Eval)});
         |
         |   Exp  exp2 = new Mult(new Divd(new Lit(5.0), new Lit(2.0)), new Lit(4.0));
         |   assertEquals("((5.0/2.0)*4.0)", ${recurseOn(Java("exp2").expression(), PrettyP)});
         |
         |   Exp  exp3 = ${convert(d2)};
         |   Exp  exp4 = ${recurseOn(Java("exp3").expression(), Simplify)};
         |   Exp  exp5 = ${convert(d1)};
         |   assertEquals (${recurseOn(Java("exp5").expression(), PrettyP)}, ${recurseOn(Java("exp4").expression(), PrettyP)});
         |}""".stripMargin).methodDeclarations()
  }
}
