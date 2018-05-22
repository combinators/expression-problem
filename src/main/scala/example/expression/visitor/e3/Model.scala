package example.expression.visitor.e3

import com.github.javaparser.ast.stmt.Statement
import expression.{Exp, Operation}
import expression.data.Eval
import expression.extensions._
import org.combinators.templating.twirl.Java
import shared.compilation.{CodeGeneratorRegistry, HasCodeGenerator}

/**
  * E3 development history
  */
trait Model extends HasCodeGenerator {
  abstract override def codeGenerator:CodeGeneratorRegistry[CodeGeneratorRegistry[Seq[Statement]]] = {
    val oldGenerator = super.codeGenerator
    CodeGeneratorRegistry.merge(
      oldGenerator,
      CodeGeneratorRegistry[CodeGeneratorRegistry[Seq[Statement]], Eval] {
        case (operationReg, eval) =>
          CodeGeneratorRegistry.merge(
            oldGenerator(eval).getOrElse(CodeGeneratorRegistry[Seq[Statement]]),
            CodeGeneratorRegistry[Seq[Statement], Neg] {
              case (evalReg, neg) =>
                Java(s"""return -1 * e.getExp().accept(this);""").statements()
            },
            CodeGeneratorRegistry[Seq[Statement], Mult] {
              case (evalReg, mult) =>
                Java(s"""return e.getLeft().accept(this) * e.getRight().accept(this);""").statements()
            },
            CodeGeneratorRegistry[Seq[Statement], Divd] {
              case (evalReg, div) =>
                Java(s"""return e.getLeft().accept(this) / e.getRight().accept(this);""").statements()
            }
          )
        },
      CodeGeneratorRegistry[CodeGeneratorRegistry[Seq[Statement]], PrettyP] {
        case (operationReg, pp) =>
          CodeGeneratorRegistry.merge(
            oldGenerator(pp).getOrElse(CodeGeneratorRegistry[Seq[Statement]]),
            CodeGeneratorRegistry[Seq[Statement], Neg] {
              case (ppReg, neg) =>
                Java(s"""return "-" + e.getExp().accept(this);""").statements()
            },
            CodeGeneratorRegistry[Seq[Statement], Mult] {
              case (ppReg, mult) =>
                Java(s"""return "(" + e.getLeft().accept(this) + "*" + e.getRight().accept(this) + ")"; """).statements()
            },
            CodeGeneratorRegistry[Seq[Statement], Divd] {
              case (ppReg, div) =>
                Java(s"""return "(" + e.getLeft().accept(this) + "/" +  e.getRight().accept(this) + ")"; """).statements()
            }
          )
      }
    )
  }
}
