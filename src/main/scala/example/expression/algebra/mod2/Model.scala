package example.expression.algebra.mod2

import com.github.javaparser.ast.stmt.Statement
import expression.{Exp, Operation}
import expression.data.{Add, Lit}
import expression.extensions.{PrettyP, Sub}
import org.combinators.templating.twirl.Java
import shared.compilation.CodeGeneratorRegistry

trait Model {

  var codeGenerator:CodeGeneratorRegistry[Seq[Statement]]

  codeGenerator = codeGenerator.merge(CodeGeneratorRegistry[Seq[Statement], (Operation,Exp)] {
    case (_:CodeGeneratorRegistry[Seq[Statement]], (_:PrettyP, _:Lit)) =>
      Java(s"""return "" + value + ""; """).statements()
  })

  codeGenerator = codeGenerator.merge(CodeGeneratorRegistry[Seq[Statement], (Operation,Exp)] {
    case (_:CodeGeneratorRegistry[Seq[Statement]],(_:PrettyP, _:Add)) =>
      Java(s"""return "(" + left.eval() + "+" + right.eval() + ")"; """).statements()
  })

  codeGenerator = codeGenerator.merge(CodeGeneratorRegistry[Seq[Statement], (Operation,Exp)] {
    case (_:CodeGeneratorRegistry[Seq[Statement]],(_:PrettyP, _:Sub)) =>
      Java(s"""return "(" + left.eval() + "-" + right.eval() + ")"; """).statements()
  })

}
