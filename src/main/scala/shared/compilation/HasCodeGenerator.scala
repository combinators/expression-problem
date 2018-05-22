package shared.compilation

import com.github.javaparser.ast.stmt.Statement

trait HasCodeGenerator {
  def codeGenerator: CodeGeneratorRegistry[CodeGeneratorRegistry[Seq[Statement]]]
}
