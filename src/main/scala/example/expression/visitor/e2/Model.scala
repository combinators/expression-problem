package example.expression.visitor.e2

import com.github.javaparser.ast.stmt.Statement
import expression.{Exp, Operation}
import expression.data._
import expression.extensions.{PrettyP, Sub}
import org.combinators.templating.twirl.Java
import shared.compilation.CodeGeneratorRegistry

/**
 * Designed knowing this comes after E1, and thus must account for Lit, Add (E0) and Sub (E1)
 */
trait Model {

  var codeGenerator:CodeGeneratorRegistry[Seq[Statement]]

  codeGenerator = codeGenerator.merge(CodeGeneratorRegistry[Seq[Statement], (Operation,Exp)] {
      case (_:CodeGeneratorRegistry[Seq[Statement]], (_:PrettyP, _:Lit)) =>
        Java(s"""return "" + e.getValue() + ""; """).statements()
    })

  codeGenerator = codeGenerator.merge(CodeGeneratorRegistry[Seq[Statement], (Operation,Exp)] {
    case (_:CodeGeneratorRegistry[Seq[Statement]],(_:PrettyP, _:Add)) =>
        Java(s"""return "(" + e.getLeft().accept(this) + "+" + e.getRight().accept(this) + ")"; """).statements()
    })

  codeGenerator = codeGenerator.merge(CodeGeneratorRegistry[Seq[Statement], (Operation,Exp)] {
    case (_:CodeGeneratorRegistry[Seq[Statement]],(_:PrettyP, _:Sub)) =>
        Java(s"""return "(" + e.getLeft().accept(this) + "-" + e.getRight().accept(this) + ")"; """).statements()
    })

}
