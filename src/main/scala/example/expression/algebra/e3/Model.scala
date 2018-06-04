package example.expression.algebra.e3

import com.github.javaparser.ast.stmt.Statement
import expression.data.Eval
import expression.extensions._
import expression.{Exp, Operation}
import org.combinators.templating.twirl.Java
import shared.compilation.CodeGeneratorRegistry

/**
  * J3 development history
  */
trait Model {

  var codeGenerator:CodeGeneratorRegistry[Seq[Statement]]

  codeGenerator = codeGenerator.merge(CodeGeneratorRegistry[Seq[Statement], (Operation,Exp)] {
    case (_:CodeGeneratorRegistry[Seq[Statement]], (_:Eval, _:Neg)) =>
      Java(s"""return - value;""").statements()
  })
  codeGenerator = codeGenerator.merge(CodeGeneratorRegistry[Seq[Statement], (Operation,Exp)] {
    case (_:CodeGeneratorRegistry[Seq[Statement]], (_:Eval, _:Mult)) =>
      Java(s"""return left.eval() * right.eval();""").statements()
  })
  codeGenerator = codeGenerator.merge(CodeGeneratorRegistry[Seq[Statement], (Operation,Exp)] {
    case (_:CodeGeneratorRegistry[Seq[Statement]], (_:Eval, _:Divd)) =>
      Java(s"""return left.eval() / right.eval();""").statements()
  })


  codeGenerator = codeGenerator.merge(CodeGeneratorRegistry[Seq[Statement], (Operation,Exp)] {
    case (_:CodeGeneratorRegistry[Seq[Statement]], (_:PrettyP, _:Neg)) =>
      Java(s"""return "-" + value.print();""").statements()
  })
  codeGenerator = codeGenerator.merge(CodeGeneratorRegistry[Seq[Statement], (Operation,Exp)] {
    case (_:CodeGeneratorRegistry[Seq[Statement]], (_:PrettyP, _:Mult)) =>
      Java(s"""return "(" + left.print() + "*" + right.print() + ")"; """).statements()
  })
  codeGenerator = codeGenerator.merge(CodeGeneratorRegistry[Seq[Statement], (Operation,Exp)] {
    case (_:CodeGeneratorRegistry[Seq[Statement]], (_:PrettyP, _:Divd)) =>
      Java(s"""return "(" + left.print() + "/" + right.print() + ")"; """).statements()
  })

}
