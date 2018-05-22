package example.expression.visitor.e1

import com.github.javaparser.ast.stmt.Statement
import expression.{Exp, Operation}
import expression.data.Eval
import expression.extensions.Sub
import org.combinators.templating.twirl.Java
import shared.compilation.{CodeGeneratorRegistry, HasCodeGenerator}

trait Model extends HasCodeGenerator {
  abstract override def codeGenerator:CodeGeneratorRegistry[CodeGeneratorRegistry[Seq[Statement]]] = {
    val oldGenerator = super.codeGenerator
    oldGenerator.merge(
      CodeGeneratorRegistry[CodeGeneratorRegistry[Seq[Statement]], Eval] {
        case (operationReg, eval) =>
          CodeGeneratorRegistry.merge(
            oldGenerator(eval).getOrElse(CodeGeneratorRegistry[Seq[Statement]]),
            CodeGeneratorRegistry[Seq[Statement], Sub] {
              case (evalReg, sub) =>
                Java(s"return e.getLeft().accept(this) - e.getRight().accept(this);").statements()
            }
          )
      }
    )
  }
}
