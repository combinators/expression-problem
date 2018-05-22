package example.expression.visitor.e0

import com.github.javaparser.ast.stmt.Statement
import expression.data.{Add, Eval, Lit}
import expression.{Exp, Operation}
import org.combinators.templating.twirl.Java
import shared.compilation.CodeGeneratorRegistry

/**
  * Each trait is stand alone, and woven into the final repository.
  */
trait Model {

  // Note: If I leave as (Operation,Exp) on line 17, then get match error.
  // If I change to (Eval,Lit), then only first case matches on Tuple2 since type information lost at runtime
  var codeGenerator:CodeGeneratorRegistry[Seq[Statement]] = CodeGeneratorRegistry.merge[Seq[Statement]](
    CodeGeneratorRegistry[Seq[Statement], (Eval,Lit)] {
      case (_:CodeGeneratorRegistry[Seq[Statement]], (_:Eval, _:Lit)) =>
        Java(s"""return value();""").statements()
    },

    CodeGeneratorRegistry[Seq[Statement], (Eval,Add)] {
      case (_:CodeGeneratorRegistry[Seq[Statement]],(_:Eval, _:Add)) =>
        Java(s"""return e.getLeft().accept(this) - e.getRight().accept(this);""").statements()
    },
  )
}
