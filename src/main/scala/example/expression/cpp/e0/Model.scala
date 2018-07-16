package example.expression.cpp.e0

import example.expression.cpp.{CPPMethod, HasCPPCodeGenerator}
import expression.data.{Add, Eval, Lit}
import shared.compilation.CodeGeneratorRegistry

/**
  * Each trait is stand alone, and woven into the final repository.
  */
trait Model extends HasCPPCodeGenerator {

  // starting point. Eval is first one
  def codeGenerator: CodeGeneratorRegistry[CodeGeneratorRegistry[CPPMethod]] = {

    // First one is defined here
    CodeGeneratorRegistry[CodeGeneratorRegistry[CPPMethod], Eval] {

      case (_, eval: Eval) =>
        CodeGeneratorRegistry.merge(

          CodeGeneratorRegistry[CPPMethod, Lit] {
            case (_, exp: Lit) =>
              val name = exp.getClass.getSimpleName
              new CPPMethod("void", s"Visit$name", s"(const $name* e)",
                "value_map_[e] = *e->getValue();")
          },

          CodeGeneratorRegistry[CPPMethod, Add] {
            case (_, exp:Add) =>
            val name = exp.getClass.getSimpleName
            new CPPMethod("void", s"Visit$name", s"(const $name* e)",
              "value_map_[e] = value_map_[e->getLeft()] + value_map_[e->getRight()];")
          }
        )
    }
  }
}
