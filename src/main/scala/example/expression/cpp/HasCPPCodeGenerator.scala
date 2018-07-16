package example.expression.cpp

import shared.compilation.CodeGeneratorRegistry


/**
  * Offers a nested code generator. Outer one is operation, inner one is by expression sub-type
  */
trait HasCPPCodeGenerator {

  def codeGenerator: CodeGeneratorRegistry[CodeGeneratorRegistry[CPPMethod]]
}
