package example.expression.visitor.e2

import com.github.javaparser.ast.stmt.Statement
import expression.{Exp, Operation}
import expression.data._
import expression.extensions.{PrettyP, Sub}
import org.combinators.templating.twirl.Java
import shared.compilation.{CodeGeneratorRegistry, HasCodeGenerator}

/**
 * Designed knowing this comes after E1, and thus must account for Lit, Add (E0) and Sub (E1)
 */
trait Model extends HasCodeGenerator {

  abstract override def codeGenerator:CodeGeneratorRegistry[CodeGeneratorRegistry[Seq[Statement]]] = {
    val oldGenerator = super.codeGenerator
    oldGenerator.merge(
      CodeGeneratorRegistry[CodeGeneratorRegistry[Seq[Statement]], PrettyP] {
        case (operationReg, pp) =>
          CodeGeneratorRegistry.merge(
            oldGenerator(pp).getOrElse(CodeGeneratorRegistry[Seq[Statement]]),
            CodeGeneratorRegistry[Seq[Statement], Lit] {
              case (ppGen, lit) =>
                Java(s"""return "" + e.getValue() + ""; """).statements()
            },
            CodeGeneratorRegistry[Seq[Statement], Add] {
              case (ppGen, add) =>
                Java(s"""return "(" + e.getLeft().accept(this) + "+" + e.getRight().accept(this) + ")"; """).statements()
            },
            CodeGeneratorRegistry[Seq[Statement], Sub] {
              case (ppGen, lit) =>
                Java(s"""return "(" + e.getLeft().accept(this) + "-" + e.getRight().accept(this) + ")"; """).statements()
            }
          )
      }
    )
  }
}
