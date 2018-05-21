package example.expression.covariant.e1

import com.github.javaparser.ast.stmt.Statement
import example.expression.covariant.{Registry, SemanticTypes}
import example.expression.j.Operators
import example.expression.{Base, ExpressionDomain}
import expression.FunctionMethod
import expression.data.{Add, Eval, Lit}
import expression.extensions.Sub
import expression.history.History
import expression.types.Types
import org.combinators.cls.interpreter.ReflectedRepository
import org.combinators.templating.twirl.Java
import shared.compilation.CodeGeneratorRegistry

trait Model extends Base with Registry with Operators with SemanticTypes {

  var evalGenerators:CodeGeneratorRegistry[Seq[Statement]]

  /** Add dynamic combinators as needed. */
    override def init[G <: ExpressionDomain](gamma: ReflectedRepository[G], history: History): ReflectedRepository[G] = {
      var updated = super.init(gamma, history)

      updated
    }

  /**
    * Code generator for SUB with eval
    */
  evalGenerators = evalGenerators.merge(CodeGeneratorRegistry[Seq[Statement], Sub] {
    case (_:CodeGeneratorRegistry[Seq[Statement]], _:Sub) =>
      Java(s"""return left().eval() - right().eval();""").statements()
  })
}
