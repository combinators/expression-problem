package example.expression.Pure

import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.stmt.Statement
import org.combinators.templating.twirl.Java

trait E4_Generator extends AbstractGenerator {
  import pure._

  abstract override def typeGenerator(tpe:types.Types) : com.github.javaparser.ast.`type`.Type = {
    tpe match {
      case el:List => Java(s"java.util.List<${typeGenerator(el.generic)}>").tpe()
      case _ => super.typeGenerator(tpe)
    }
  }

  abstract override def methodBodyGenerator(exp:expressions.Exp)(op:Operation): Seq[Statement] = {
    // generate the actual body
    op match {
      case Simpify => {
        exp match {
          case Lit => Java (s"return this;").statements()
          case Add => Java(s"""|double leftVal = left.$EVAL();
                               |double rightVal = right.$EVAL();
                               |if ((leftVal == 0 && rightVal == 0) || (leftVal + rightVal == 0)) {
                               |  return new Lit(0.0);
                               |} else if (leftVal == 0) {
                               |  return right.$SIMPLIFY();
                               |} else if (rightVal == 0) {
                               |  return left.$SIMPLIFY();
                               |} else {
                               |  return new Add(left.$SIMPLIFY(), right.$SIMPLIFY());
                               |}""".stripMargin).statements()
          case Sub => Java(s"""
                              |if (left.$EVAL() == right.$EVAL()) {
                              |  return new Lit(0.0);
                              |} else {
                              |  return new Sub(left.$SIMPLIFY(), right.$SIMPLIFY());
                              |}
                              |""".stripMargin).statements()
          case Mult => Java(s"""
                               |double leftVal = left.$EVAL();
                               |double rightVal = right.$EVAL();
                               |if (leftVal == 0 || rightVal == 0) {
                               |  return new Lit(0.0);
                               |} else if (leftVal == 1) {
                               |  return right.$SIMPLIFY();
                               |} else if (rightVal == 1) {
                               |  return left.$SIMPLIFY();
                               |} else {
                               |  return new Mult(left.$SIMPLIFY(), right.$SIMPLIFY());
                               |}
                               |""".stripMargin).statements()
          case Divd => Java(s"""
                               |double leftVal = left.$EVAL();
                               |double rightVal = right.$EVAL();
                               |if (leftVal == 0) {
                               |  return new Lit(0.0);
                               |} else if (rightVal == 1) {
                               |  return left.$SIMPLIFY();
                               |} else if (leftVal == rightVal) {
                               |  return new Lit(1.0);
                               |} else if (leftVal == -rightVal) {
                               |  return new Lit(-1.0);
                               |} else {
                               |  return new Divd(left.$SIMPLIFY(), right.$SIMPLIFY());
                               |}
                               |""".stripMargin).statements()
          case Neg => Java(s"""
                              |if (exp.$EVAL() == 0) {
                              |  return new Lit(0.0);
                              |} else {
                              |  return this;
                              |}""".stripMargin).statements()
          case _ => super.methodBodyGenerator(exp)(op)
        }
      }

      case Collect => {
        val collectStatements: Seq[Statement] =
          Java(
            s"""|java.util.List<Double> list = left.$COLLECT();
                |list.addAll(right.$COLLECT());
                |return list;
                |""".stripMargin).statements()

        exp match {
          case Add | Sub | Mult | Divd => collectStatements
          case Lit => Java(s"return java.util.Collections.singletonList($VALUE);").statements()
          case Neg => Java(s"return exp.$COLLECT();").statements()
          case _ => super.methodBodyGenerator(exp)(op)
        }
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
         |   $BASE  exp1 = new Neg(new Lit(1.0));
         |   assertEquals("-1.0", exp1.$PRINT());
         |   assertEquals(-1.0, exp1.$EVAL());
         |
         |   $BASE  exp2 = new Mult(new Divd(new Lit(5.0), new Lit(2.0)), new Lit(4.0));
         |   assertEquals("((5.0/2.0)*4.0)", exp2.$PRINT());
         |
         |   $BASE  exp3 = ${convert(d2)};
         |   $BASE  exp4 = exp3.$SIMPLIFY();
         |   $BASE  exp5 = ${convert(d1)};
         |   assertEquals (exp5.$PRINT(), exp4.$PRINT());
         |}""".stripMargin).methodDeclarations()
  }
}