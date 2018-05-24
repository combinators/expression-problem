package example.expression.covariant.e3

import com.github.javaparser.ast.stmt.Statement
import expression.data.Eval
import expression.extensions._
import org.combinators.templating.twirl.Java
import shared.compilation.{CodeGeneratorRegistry, HasCodeGenerator}

trait Model extends HasCodeGenerator {
  abstract override def codeGenerator:CodeGeneratorRegistry[CodeGeneratorRegistry[Seq[Statement]]] = {
    val oldGenerator = super.codeGenerator

    // it is critical that the new changes are merged before old ones
    CodeGeneratorRegistry.merge(

      CodeGeneratorRegistry[CodeGeneratorRegistry[Seq[Statement]], Eval] {
        case (_, eval:Eval) =>

          CodeGeneratorRegistry.merge(
            oldGenerator(eval).getOrElse(CodeGeneratorRegistry[Seq[Statement]]),

            CodeGeneratorRegistry[Seq[Statement], Neg] {
              case (_, dataty:Neg) =>
                Java(s"""return -exp().eval();""").statements()
            },
            CodeGeneratorRegistry[Seq[Statement], Mult] {
              case (_, dataty:Mult) =>
                Java(s"""return left().eval() * right().eval();""").statements()
            },
            CodeGeneratorRegistry[Seq[Statement], Divd] {
              case (_, dataty:Divd) =>
                Java(s"""return left().eval() / right().eval();""").statements()
            }
          )
      },

      CodeGeneratorRegistry[CodeGeneratorRegistry[Seq[Statement]], PrettyP] {
        case (_, pp:PrettyP) =>
          CodeGeneratorRegistry.merge(
            oldGenerator(pp).getOrElse(CodeGeneratorRegistry[Seq[Statement]]),
            CodeGeneratorRegistry[Seq[Statement], Neg] {
              case (_, dataty:Neg) =>
                Java(s"""return "-" + exp().print();""").statements()
            },
            CodeGeneratorRegistry[Seq[Statement], Mult] {
              case (_, dataty:Mult) =>
                Java(s"""return "(" + left().print() + "*" + right().print() + ")";""").statements()
            },
            CodeGeneratorRegistry[Seq[Statement], Divd] {
              case (_, dataty:Divd) =>
                Java(s"""return "(" + left().print() + "/" + right().print() + ")";""").statements()
            }
          )
      },

      oldGenerator
    )
  }

}
