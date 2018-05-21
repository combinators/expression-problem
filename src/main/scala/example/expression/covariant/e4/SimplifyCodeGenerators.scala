package example.expression.covariant.e4

import com.github.javaparser.ast.stmt.Statement
import expression.data._
import expression.extensions.{Divd, Mult, Neg, Sub}
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
class SimplifyCodeGenerators(subtypes:String)  {

  /**
    * Code generator for reproducing the structure of the covariant pattern invocation for simplify.
    *
    * What is TRULY challenging is construction Lit replacements, with the desired subtype, which
    * is only known by the invoking code.
    */
  var simplifyGenerators:CodeGeneratorRegistry[Seq[Statement]] = CodeGeneratorRegistry.merge[Seq[Statement]](
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
               |  return new Add${subtypes}Final(($subtypes)left().simplify(), ($subtypes)right().simplify());
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
                |  return new Sub${subtypes}Final(($subtypes)left().simplify(), ($subtypes)right().simplify());
                |}
                |""".stripMargin).statements()
    },

    CodeGeneratorRegistry[Seq[Statement], Neg] {
      case (_:CodeGeneratorRegistry[Seq[Statement]], _:Neg) =>
        Java(s"""
                |if (eval() == 0) {
                |  return new Lit${subtypes}Final(0.0);
                |} else {
                |  return new Neg${subtypes}Final(($subtypes)exp().simplify());
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
              |  return new Mult${subtypes}Final(($subtypes)left().simplify(), ($subtypes)right().simplify());
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
                |  return new Divd${subtypes}Final(($subtypes)left().simplify(), ($subtypes)right().simplify());
                |}
                |""".stripMargin).statements()
    }
  )
}
