package example.expression.covariant

import example.expression.j.Operators
import org.combinators.templating.twirl.Java
import shared.compilation.CodeGeneratorRegistry

/**
  * Within Co-variant solution, the classes of the instances changes, depending upon the required operations.
  * Since these instances are constructed solely within test cases, then
  */
class InstanceCodeGenerators(subTypes:String) extends Operators {

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
  val instanceGenerators:CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression] = CodeGeneratorRegistry.merge[com.github.javaparser.ast.expr.Expression](
    CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression, expression.instances.Lit] {
      case (_:CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression], lit:expression.instances.Lit) => {
        // must choose the most specific sub-types available, depending upon the operations
        //val subTypes:String = computeSubTypes()
        Java(s"""new Lit${subTypes}Final(${lit.value})""").expression[com.github.javaparser.ast.expr.Expression]
      }
    },

    CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression, expression.instances.BinaryExp] {
      case (registry:CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression], binary:expression.instances.BinaryExp) => {
        val Type:String = binary.self.getClass.getSimpleName
        val left:Option[com.github.javaparser.ast.expr.Expression] = registry(binary.left)
        val right:Option[com.github.javaparser.ast.expr.Expression] = registry(binary.right)
        //val subTypes:String = computeSubTypes()
        if (left.isDefined && right.isDefined) {
          Java(s"""new ${Type}${subTypes}Final(${left.get}, ${right.get})""").expression[com.github.javaparser.ast.expr.Expression]
        } else {
          Java("""false""").expression[com.github.javaparser.ast.expr.Expression]
        }
      }
    },

    CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression, expression.instances.UnaryExp] {
      case (registry:CodeGeneratorRegistry[com.github.javaparser.ast.expr.Expression], unary:expression.instances.UnaryExp) => {
        val Type:String = unary.self.getClass.getSimpleName
        val inner:Option[com.github.javaparser.ast.expr.Expression] = registry(unary.exp)
        //val subTypes:String = computeSubTypes()
        if (inner.isDefined) {
          Java(s"""new ${Type}${subTypes}Final(${inner.get})""").expression[com.github.javaparser.ast.expr.Expression]
        } else {
          Java("false").expression[com.github.javaparser.ast.expr.Expression]
        }
      }
    },
  )
}
