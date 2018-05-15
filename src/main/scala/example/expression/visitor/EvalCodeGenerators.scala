package example.expression.visitor

import example.expression.j.Operators
import expression.data._
import org.combinators.templating.twirl.Java
import shared.compilation.CodeGeneratorRegistry

/**
  * Eval implementation using standard data type
  *
  * Not yet extensible to new data types that fall outside of the Lit/BinaryExp/UnaryExp hierarchy.
  */
trait EvalCodeGenerators extends Operators {

  /**
    * Code generator for reproducing the structure of the visitor pattern invocation for eval.
    */
  val evalGenerators:CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression] = CodeGeneratorRegistry.merge[com.github.javaparser.ast.expr.Expression](
    CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression, Lit] {
      case (_:CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression], _:Lit) =>
        Java(s"""e.getValue()""").expression[com.github.javaparser.ast.expr.Expression]
    },

    CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression, BinaryExp] {
      case (_:CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression], binary:BinaryExp) =>
        Java(s"""e.getLeft().accept(this) ${getBinaryOperator(binary).asString} e.getRight().accept(this)""").expression[com.github.javaparser.ast.expr.Expression]
    },

    CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression, UnaryExp] {
      case (_:CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression], unary:UnaryExp) =>
        Java(s"""${getUnaryOperator(unary).asString} e.getExp().accept(this)""").expression[com.github.javaparser.ast.expr.Expression]()
    },
  )
}
