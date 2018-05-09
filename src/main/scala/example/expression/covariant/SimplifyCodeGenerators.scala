package example.expression.covariant

import com.github.javaparser.ast.stmt.Statement
import example.expression.j.MethodMapper
import expression.data._
import expression.extensions.{Divd, Mult, Neg, Sub}
import expression.{DomainModel, Exp}
import org.combinators.cls.types.Type
import org.combinators.cls.types.syntax._
import org.combinators.templating.twirl.Java
import shared.compilation.CodeGeneratorRegistry

/**
  * Common code generators for the covariant solution.
  *
  * Customized to know about the mode recent model domain. Simplification reconstructs the
  * expression using Lit values to represent basic values (1 and 0, for example). However,
  * depending upon the context of the expression, one needs different subclasses. Use
  * the subtypes as the context.
  */
class SimplifyCodeGenerators(model:DomainModel, subtypes:String)  {

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
        Java(s"""return this;""").statements() // nothing to simplify
    },

    CodeGeneratorRegistry[Seq[Statement], Add] {
      case (_:CodeGeneratorRegistry[Seq[Statement]], _:Add) =>
        Java(s"""
               |double leftVal = left().eval();
               |double rightVal = right().eval();
               |if (leftVal + rightVal == 0) {
               |  return new Lit${subtypes}Final(0.0);
               |} else if (leftVal == 0) {
               |  return right();
               |} else if (rightVal == 0) {
               |  return left();
               |} else {
               |  return new Add${subtypes}Final((${subtypes})left().simplify(), (${subtypes})right().simplify());
               |}
               |""".stripMargin).statements()
    },

    CodeGeneratorRegistry[Seq[Statement], Sub] {
      case (_:CodeGeneratorRegistry[Seq[Statement]], _:Sub) =>
        Java(s"""
                |double leftVal = left().eval();
                |double rightVal = right().eval();
                |if (leftVal == rightVal) {
                |  return new Lit${subtypes}Final(0.0);
                |} else {
                |  return new Sub${subtypes}Final((${subtypes})left().simplify(), (${subtypes})right().simplify());
                |}
                |""".stripMargin).statements()
    },

    CodeGeneratorRegistry[Seq[Statement], Neg] {
      case (_:CodeGeneratorRegistry[Seq[Statement]], _:Neg) =>
        Java(s"""
                |if (eval() == 0) {
                |  return new Lit${subtypes}Final(0.0);
                |} else {
                |  return new Neg${subtypes}Final((${subtypes})exp().simplify());
                |}""".stripMargin).statements()
    },

    CodeGeneratorRegistry[Seq[Statement], Mult] {
      case (_:CodeGeneratorRegistry[Seq[Statement]], _:Mult) =>
        val str:String = s"""
              |double leftVal = left().eval();
              |double rightVal = right().eval();
              |if (leftVal == 0 || rightVal == 0) {
              |  return new Lit${subtypes}Final(0.0);
              |} else if (leftVal == 1.0) {
              |  return right();
              |} else if (rightVal == 1.0) {
              |  return left();
              |} else {
              |  return new Mult${subtypes}Final((${subtypes})left().simplify(), (${subtypes})right().simplify());
              |}
              |""".stripMargin
      println(str)
      Java(str).statements()
    },

    CodeGeneratorRegistry[Seq[Statement], Divd] {
      case (_:CodeGeneratorRegistry[Seq[Statement]], _:Divd) =>
        Java(s"""
                |double leftVal = left().eval();
                |double rightVal = right().eval();
                |if (leftVal == 0) {
                |    return new Lit${subtypes}Final(0.0);
                |} else if (rightVal == 1) {
                |    return left().simplify();
                |} else if (leftVal == rightVal) {
                |    return new Lit${subtypes}Final(1.0);
                |} else if (leftVal == -rightVal) {
                |    return new Lit${subtypes}Final(-1.0);
                |} else {
                |  return new Divd${subtypes}Final((${subtypes})left().simplify(), (${subtypes})right().simplify());
                |}
                |""".stripMargin).statements()
    }
  )
}
