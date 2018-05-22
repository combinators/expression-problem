package example.expression.algebra.e1

import com.github.javaparser.ast.stmt.Statement
import expression.{Exp, Operation}
import expression.data.Eval
import expression.extensions.Sub
import org.combinators.templating.twirl.Java
import shared.compilation.CodeGeneratorRegistry

trait Model {

  var codeGenerator:CodeGeneratorRegistry[Seq[Statement]]

  codeGenerator = codeGenerator.merge(CodeGeneratorRegistry[Seq[Statement], (Operation,Exp)] {
    case (_:CodeGeneratorRegistry[Seq[Statement]], (_:Eval, _:Sub)) =>
      Java(s"return left.eval() - right.eval();").statements()
  })

}
