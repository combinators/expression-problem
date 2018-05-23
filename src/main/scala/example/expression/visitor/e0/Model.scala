package example.expression.visitor.e0

import com.github.javaparser.ast.stmt.Statement
import expression.data.{Add, Eval, Lit}
import org.combinators.templating.twirl.Java
import shared.compilation.{CodeGeneratorRegistry, HasCodeGenerator}

/**
  * Each trait is stand alone, and woven into the final repository.
  */
trait Model extends HasCodeGenerator {

  // starting point. Eval is first one
  def codeGenerator:CodeGeneratorRegistry[CodeGeneratorRegistry[Seq[Statement]]] = {

    // First one is defined here
    CodeGeneratorRegistry[CodeGeneratorRegistry[Seq[Statement]], Eval] {

        case (_, eval:Eval) =>
          CodeGeneratorRegistry.merge(

            CodeGeneratorRegistry[Seq[Statement], Lit] {
              case (_, dataty:Lit) =>
                Java(s"""return e.getValue();""").statements()
            },

            CodeGeneratorRegistry[Seq[Statement], Add] {
              case (_, dataty:Add) =>
                Java(s"""return e.getLeft().accept(this) + e.getRight().accept(this);""").statements()
            }
          )

      }
  }
}
