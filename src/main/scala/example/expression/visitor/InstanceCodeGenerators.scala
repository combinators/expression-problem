package example.expression.visitor

import example.expression.j.Operators
import org.combinators.templating.twirl.Java
import shared.compilation.CodeGeneratorRegistry

/**
  * Used solely to instantiate expression instances that are restricted to {Lit, BinaryExp, UnaryExp}.
  *
  * Not extensible to new data types that fall outside of the Lit/BinaryExp/UnaryExp hierarchy.
  */
trait InstanceCodeGenerators extends Operators {

  /**
    * Code generator for building up the structure of the expression using classes
    *
    * new BinaryExp(new Add, new Lit(new Lit, 1), new Lit(new Lit, 2))  -->
    *
    *   Add add = new Add(new Lit(1), new Lit(2));
    *
    *    // ( ( 5 * 7 ) + ( 8 / 9 ) )
    *
    * new BinaryExp(new Add(),
            new expression.instances.BinaryExp(new Mult(),
                new expression.instances.Lit(new Lit(),5),
                new expression.instances.Lit(new Lit(),7))
            new expression.instances.BinaryExp(new Divd(),
                new expression.instances.Lit(new Lit(),8),
                new expression.instances.Lit(new Lit(),9));
    */
  object defaultInstance {
    val instanceGenerators: CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression] = CodeGeneratorRegistry.merge[com.github.javaparser.ast.expr.Expression](
      CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression, expression.instances.Lit] {
        case (_: CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression], lit: expression.instances.Lit) =>
          Java(s"""new Lit(Double.valueOf(${lit.value}))""").expression[com.github.javaparser.ast.expr.Expression]
      },

      CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression, expression.instances.BinaryExp] {
        case (registry: CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression], binary: expression.instances.BinaryExp) =>
          val Type: String = binary.self.getClass.getSimpleName
          val left: Option[com.github.javaparser.ast.expr.Expression] = registry(binary.left)
          val right: Option[com.github.javaparser.ast.expr.Expression] = registry(binary.right)
          if (left.isDefined && right.isDefined) {
            Java(s"""new $Type(${left.get}, ${right.get})""").expression[com.github.javaparser.ast.expr.Expression]
          } else {
            Java("""false""").expression[com.github.javaparser.ast.expr.Expression]
          }
      },

      CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression, expression.instances.UnaryExp] {
        case (registry: CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression], unary: expression.instances.UnaryExp) =>
          val Type: String = unary.self.getClass.getSimpleName
          val inner: Option[com.github.javaparser.ast.expr.Expression] = registry(unary.exp)
          if (inner.isDefined) {
            Java(s"""new $Type(${inner.get})""").expression[com.github.javaparser.ast.expr.Expression]
          } else {
            Java("false").expression[com.github.javaparser.ast.expr.Expression]
          }
      },
    )
  }
}
