package example.expression.algebra.mod0

import com.github.javaparser.ast.stmt.Statement
import expression.{Exp, Operation}
import expression.data._
import org.combinators.templating.twirl.Java
import shared.compilation.CodeGeneratorRegistry

trait Model {

  var codeGenerator:CodeGeneratorRegistry[Seq[Statement]] = CodeGeneratorRegistry.merge[Seq[Statement]](
    CodeGeneratorRegistry[Seq[Statement], (Operation,Exp)] {
      case (_:CodeGeneratorRegistry[Seq[Statement]], (_:Eval, _:Lit)) =>
        Java(s"""return value;""").statements()
    },

    CodeGeneratorRegistry[Seq[Statement], (Operation,Exp)] {
      case (_:CodeGeneratorRegistry[Seq[Statement]],(_:Eval, _:Add)) =>
        Java(s"""return left.eval() + right.eval();""").statements()
    },
  )

}
