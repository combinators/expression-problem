package shared.compilation

import com.github.javaparser.ast.stmt.Statement

/**
  * Offers a nested code generator. Outer one is operation, inner one is by expression sub-type
  */
trait HasCodeGenerator {
  def codeGenerator: CodeGeneratorRegistry[CodeGeneratorRegistry[Seq[Statement]]]
}
