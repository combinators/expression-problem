package example.expression.j

import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.expr.Expression
import com.github.javaparser.ast.stmt.Statement
import example.expression.domain.Domain
import org.combinators.templating.twirl.Java

trait Simplify extends AbstractGenerator with TestGenerator {
  val domain:Domain
  import domain._

  abstract override def typeGenerator(tpe:types.Types) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case el:List => Java(s"java.util.List<${typeGenerator(el.generic)}>").tpe()
      case _ => super.typeGenerator(tpe)
    }
  }

  abstract override def methodBodyGenerator(exp:expressions.Exp)(op:Operation): Seq[Statement] = {
    // generate the actual body
    op match {
      case Simplify => {
        exp match {
          case Lit => Java (s"return this;").statements()
          case Add => Java(s"""|double leftVal = ${oper("left", Eval)};
                               |double rightVal = ${oper("right", Eval)};
                               |if ((leftVal == 0 && rightVal == 0) || (leftVal + rightVal == 0)) {
                               |  return new Lit(0.0);
                               |} else if (leftVal == 0) {
                               |  return ${oper("right", Simplify)};
                               |} else if (rightVal == 0) {
                               |  return ${oper("left", Simplify)};
                               |} else {
                               |  return new Add(${oper("left", Simplify)}, ${oper("right", Simplify)});
                               |}""".stripMargin).statements()
          case Sub => Java(s"""
                              |if (${oper("left", Eval)} == ${oper("right", Eval)}) {
                              |  return new Lit(0.0);
                              |} else {
                              |  return new Sub(${oper("left", Simplify)}, ${oper("right", Simplify)});
                              |}
                              |""".stripMargin).statements()
          case Mult => Java(s"""
                               |double leftVal = ${oper("left", Eval)};
                               |double rightVal = ${oper("right", Eval)};
                               |if (leftVal == 0 || rightVal == 0) {
                               |  return new Lit(0.0);
                               |} else if (leftVal == 1) {
                               |  return ${oper("right", Simplify)};
                               |} else if (rightVal == 1) {
                               |  return ${oper("left", Simplify)};
                               |} else {
                               |  return new Mult(${oper("left", Simplify)}, ${oper("right", Simplify)});
                               |}
                               |""".stripMargin).statements()
          case Divd => Java(s"""
                               |double leftVal = ${oper("left", Eval)};
                               |double rightVal = ${oper("right", Eval)};
                               |if (leftVal == 0) {
                               |  return new Lit(0.0);
                               |} else if (rightVal == 1) {
                               |  return ${oper("left", Simplify)};
                               |} else if (leftVal == rightVal) {
                               |  return new Lit(1.0);
                               |} else if (leftVal == -rightVal) {
                               |  return new Lit(-1.0);
                               |} else {
                               |  return new Divd(${oper("left", Simplify)}, ${oper("right", Simplify)});
                               |}
                               |""".stripMargin).statements()
          case Neg => Java(s"""
                              |if (${oper("exp", Eval)} == 0) {
                              |  return new Lit(0.0);
                              |} else {
                              |  return this;
                              |}""".stripMargin).statements()
          case _ => super.methodBodyGenerator(exp)(op)
        }
      }

      case Collect =>
        val collectStatements: Seq[Statement] =
          Java(
            s"""|java.util.List<Double> list = ${oper("left", Collect)};
                |list.addAll(${oper("right", Collect)});
                |return list;
                |""".stripMargin).statements()

        exp match {
          case Add | Sub | Mult | Divd => collectStatements
          case Lit => Java(s"return java.util.Collections.singletonList($VALUE);").statements()
          case Neg => Java(s"return ${oper("exp", Collect)};").statements()
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
         |   assertEquals("-1.0", ${oper("exp1", PrettyP)});
         |   assertEquals(-1.0, ${oper("exp1", Eval)});
         |
         |   Exp  exp2 = new Mult(new Divd(new Lit(5.0), new Lit(2.0)), new Lit(4.0));
         |   assertEquals("((5.0/2.0)*4.0)", ${oper("exp2", PrettyP)});
         |
         |   Exp  exp3 = ${convert(d2)};
         |   Exp  exp4 = ${oper("exp3", Simplify)};
         |   Exp  exp5 = ${convert(d1)};
         |   assertEquals (${oper("exp5", PrettyP)}, ${oper("exp4", PrettyP)});
         |}""".stripMargin).methodDeclarations()
  }
}
