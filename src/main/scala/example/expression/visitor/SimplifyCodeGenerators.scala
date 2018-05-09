package example.expression.visitor

import com.github.javaparser.ast.stmt.Statement
import expression.data._
import expression.extensions.{Divd, Mult, Neg, Sub}
import expression.{DomainModel, Exp}
import org.combinators.templating.twirl.Java
import shared.compilation.CodeGeneratorRegistry

/**
  * Common code generators for the visitor solution.
  */
class SimplifyCodeGenerators(model:DomainModel)  {

  /**
    * Return the operator to use for a given binary exp type in Java.
    *
    * @param exp Exp subclass that corresponds to a binary operator.
    * @return
    */
  def getBinaryOperator( exp:Exp) : com.github.javaparser.ast.expr.BinaryExpr.Operator = {
    exp match {

      case _:Add => com.github.javaparser.ast.expr.BinaryExpr.Operator.PLUS
      case _:Sub => com.github.javaparser.ast.expr.BinaryExpr.Operator.MINUS
      case _:Mult => com.github.javaparser.ast.expr.BinaryExpr.Operator.MULTIPLY
      case _:Divd => com.github.javaparser.ast.expr.BinaryExpr.Operator.DIVIDE

      case _ => null
    }
  }

  /**
    * Return the operator to use for a given unary exp type in Java.
    *
    * @param exp  Exp subclass that corresponds to a unary operator.
    * @return
    */
  def getUnaryOperator( exp:Exp) : com.github.javaparser.ast.expr.UnaryExpr.Operator = {
    exp match {

      case _:Neg => com.github.javaparser.ast.expr.UnaryExpr.Operator.MINUS

      case _ => null
    }
  }

  /**
    * Code generator for reproducing the structure of the covariant pattern invocation for simplify.
    *
    * What is TRULY challenging is construction Lit replacements, with the desired subtype, which
    * is only known by the invoking code.
    */
  val simplifyGenerators:CodeGeneratorRegistry[Seq[Statement]] = CodeGeneratorRegistry.merge[Seq[Statement]](
    CodeGeneratorRegistry[Seq[Statement], Lit] {
      case (_:CodeGeneratorRegistry[Seq[Statement]], _:Lit) =>
        Java(s"""return e;""").statements()
    },

    CodeGeneratorRegistry[Seq[Statement], Add] {
      case (_:CodeGeneratorRegistry[Seq[Statement]], _:Add) =>
        Java(s"""
                |double leftVal = e.getLeft().accept(new Eval());
                |double rightVal = e.getRight().accept(new Eval());
                |if ((leftVal == 0 && rightVal == 0) || (leftVal + rightVal == 0)) {
                |  return new Lit(0.0);
                |} else if (leftVal == 0) {
                |  return e.getRight().accept(this);
                |} else if (rightVal == 0) {
                |  return e.getLeft().accept(this);
                |} else {
                |  return new Add(e.getLeft().accept(this), e.getRight().accept(this));
                |}
                |""".stripMargin).statements()
    },

    CodeGeneratorRegistry[Seq[Statement], Sub] {
      case (_:CodeGeneratorRegistry[Seq[Statement]], _:Sub) =>
        Java(s"""
                |if (e.getLeft().accept(new Eval()).equals(e.getRight().accept(new Eval()))) {
                |  return new Lit(0.0);
                |} else {
                |  return new Sub(e.getLeft().accept(this), e.getRight().accept(this));
                |}
                |""".stripMargin).statements()
    },

    CodeGeneratorRegistry[Seq[Statement], Neg] {
      case (_:CodeGeneratorRegistry[Seq[Statement]], _:Neg) =>
        Java(s"""
                |if (e.getExp().accept(new Eval()) == 0) {
                |  return new Lit(0.0);
                |} else {
                |  return e;
                |}""".stripMargin).statements()
    },

    CodeGeneratorRegistry[Seq[Statement], Mult] {
      case (_:CodeGeneratorRegistry[Seq[Statement]], _:Mult) =>
        Java(s"""
              |double leftVal = e.getLeft().accept(new Eval());
              |double rightVal = e.getRight().accept(new Eval());
              |if (leftVal == 0 || rightVal == 0) {
              |  return new Lit(0.0);
              |} else if (leftVal == 1) {
              |  return e.getRight().accept(this);
              |} else if (rightVal == 1) {
              |  return e.getLeft().accept(this);
              |} else {
              |  return new Mult(e.getLeft().accept(this), e.getRight().accept(this));
              |}
              |""".stripMargin).statements()
    },

    CodeGeneratorRegistry[Seq[Statement], Divd] {
      case (_:CodeGeneratorRegistry[Seq[Statement]], _:Divd) =>
        Java(s"""
                |double leftVal = e.getLeft().accept(new Eval());
                |double rightVal = e.getRight().accept(new Eval());
                |if (leftVal == 0) {
                |  return new Lit(0.0);
                |} else if (rightVal == 1) {
                |  return e.getLeft().accept(this);
                |} else if (leftVal == rightVal) {
                |  return new Lit(1.0);
                |} else if (leftVal == -rightVal) {
                |  return new Lit(-1.0);
                |} else {
                |  return new Divd(e.getLeft().accept(this), e.getRight().accept(this));
                |}
                |""".stripMargin).statements()
    }
  )
}
