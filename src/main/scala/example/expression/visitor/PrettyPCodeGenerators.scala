package example.expression.visitor

import example.expression.j.Operators
import expression.DomainModel
import expression.data._
import org.combinators.templating.twirl.Java
import shared.compilation.CodeGeneratorRegistry

/**
  * PrettyP implementation using standard data type
  *
  * Not yet extensible to new data types that fall outside of the Lit/BinaryExp/UnaryExp hierarchy.
  */
trait PrettyPCodeGenerators extends Operators {

  /**
    * Code generator for reproducing the structure of the visitor pattern invocation for prettyP.
    */
  val prettypGenerators:CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression] = CodeGeneratorRegistry.merge[com.github.javaparser.ast.expr.Expression](
    CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression, Lit] {
      case (_:CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression], _:Lit) =>
        Java(s"""|"" + e.getValue() + ""
                 |""".stripMargin).expression[com.github.javaparser.ast.expr.Expression]
    },

    CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression, BinaryExp] {
      case (_:CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression], binary:BinaryExp) =>
        Java(
          s"""|"(" + e.getLeft().accept(this) + "${getBinaryOperator(binary).asString}" + e.getRight().accept(this) + ")"
              |""".stripMargin).expression[com.github.javaparser.ast.expr.Expression]
    },

    CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression, UnaryExp] {
      case (_:CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression], unary:UnaryExp) =>
        Java(
          s"""|"${getUnaryOperator(unary).asString}" + e.getExp().accept(this)
              |""".stripMargin).expression[com.github.javaparser.ast.expr.Expression]
    },
  )
}
