package example.expression.visitor.e3

import com.github.javaparser.ast.stmt.Statement
import expression.{Exp, Operation}
import expression.data.Eval
import expression.extensions._
import org.combinators.templating.twirl.Java
import shared.compilation.CodeGeneratorRegistry

/**
  * E3 development history
  */
trait Model {

  var codeGenerator:CodeGeneratorRegistry[Seq[Statement]]

  codeGenerator = codeGenerator.merge(CodeGeneratorRegistry[Seq[Statement], (Operation,Exp)] {
    case (_:CodeGeneratorRegistry[Seq[Statement]], (_:Eval, _:Neg)) =>
      Java(s"""return - e.getExp().accept(this);""").statements()
  })
  codeGenerator = codeGenerator.merge(CodeGeneratorRegistry[Seq[Statement], (Operation,Exp)] {
    case (_:CodeGeneratorRegistry[Seq[Statement]], (_:Eval, _:Mult)) =>
      Java(s"""return e.getLeft().accept(this) * e.getRight().accept(this);""").statements()
  })
  codeGenerator = codeGenerator.merge(CodeGeneratorRegistry[Seq[Statement], (Operation,Exp)] {
    case (_:CodeGeneratorRegistry[Seq[Statement]], (_:Eval, _:Divd)) =>
      Java(s"""return e.getLeft().accept(this) / e.getRight().accept(this);""").statements()
  })


  codeGenerator = codeGenerator.merge(CodeGeneratorRegistry[Seq[Statement], (Operation,Exp)] {
    case (_:CodeGeneratorRegistry[Seq[Statement]], (_:PrettyP, _:Neg)) =>
      Java(s"""return "-" + e.getExp().accept(this);""").statements()
  })
  codeGenerator = codeGenerator.merge(CodeGeneratorRegistry[Seq[Statement], (Operation,Exp)] {
    case (_:CodeGeneratorRegistry[Seq[Statement]], (_:PrettyP, _:Mult)) =>
      Java(s"""return "(" + e.getLeft().accept(this) + "*" + e.getRight().accept(this) + ")"; """).statements()
  })
  codeGenerator = codeGenerator.merge(CodeGeneratorRegistry[Seq[Statement], (Operation,Exp)] {
    case (_:CodeGeneratorRegistry[Seq[Statement]], (_:PrettyP, _:Divd)) =>
      Java(s"""return "(" + e.getLeft().accept(this) + "/" +  e.getRight().accept(this) + ")"; """).statements()
  })

}
