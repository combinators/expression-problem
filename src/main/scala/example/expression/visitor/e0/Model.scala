package example.expression.visitor.e0

import com.github.javaparser.ast.stmt.Statement
import expression.data.{Add, Eval, Lit}
import expression.extensions.Sub
import org.combinators.templating.twirl.Java
import shared.compilation.{CodeGeneratorRegistry, HasCodeGenerator}

/**
  * Each trait is stand alone, and woven into the final repository.
  */
trait Model extends HasCodeGenerator {
  abstract override def codeGenerator:CodeGeneratorRegistry[CodeGeneratorRegistry[Seq[Statement]]] = {
    val oldGenerator = super.codeGenerator
    oldGenerator.merge(
      CodeGeneratorRegistry[CodeGeneratorRegistry[Seq[Statement]], Eval] {
        case (operationReg, eval) =>
          CodeGeneratorRegistry.merge(
            oldGenerator(eval).getOrElse(CodeGeneratorRegistry[Seq[Statement]]),
            CodeGeneratorRegistry[Seq[Statement], Lit] {
              case (evalReg, dataty) =>
                Java(s"""return value();""").statements()
            },
            CodeGeneratorRegistry[Seq[Statement], Add] {
              case (evalReg, dataty) =>
                Java(s"""return e.getLeft().accept(this) + e.getRight().accept(this);""").statements()
            }
          )
      })
  }
}
