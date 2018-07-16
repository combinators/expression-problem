package example.expression.covariant.e1

import com.github.javaparser.ast.stmt.Statement
import expression.data.Eval
import expression.extensions.Sub
import org.combinators.templating.twirl.Java
import shared.compilation.{CodeGeneratorRegistry, HasCodeGenerator}

trait Model extends HasCodeGenerator  {

  abstract override def codeGenerator:CodeGeneratorRegistry[CodeGeneratorRegistry[Seq[Statement]]] = {
    val oldGenerator = super.codeGenerator

    // it is critical that the new changes are merged before old ones
    CodeGeneratorRegistry.merge(
      CodeGeneratorRegistry[CodeGeneratorRegistry[Seq[Statement]], Eval] {
        case (_, eval:Eval) =>

          val oldGen:CodeGeneratorRegistry[Seq[Statement]] = oldGenerator(eval).getOrElse(CodeGeneratorRegistry[Seq[Statement]])

          oldGen.merge(CodeGeneratorRegistry[Seq[Statement], Sub] {
            case (_:CodeGeneratorRegistry[Seq[Statement]], _:Sub) =>
              Java(s"return left().eval() - right().eval();").statements()
          }
          )
      },

      oldGenerator
    )
  }
}
