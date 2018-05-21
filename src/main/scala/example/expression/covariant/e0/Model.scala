package example.expression.covariant.e0

import com.github.javaparser.ast.stmt.Statement
import example.expression.covariant.{Registry, SemanticTypes}
import example.expression.j.Operators
import example.expression.{Base, ExpressionDomain}
import expression.FunctionMethod
import expression.data._
import expression.history.History
import expression.types.Types
import org.combinators.cls.interpreter.ReflectedRepository
import org.combinators.templating.twirl.Java
import shared.compilation.CodeGeneratorRegistry

trait Model extends Base with Registry with Operators with SemanticTypes {

    /** Add dynamic combinators as needed. */
    override def init[G <: ExpressionDomain](gamma: ReflectedRepository[G], history: History): ReflectedRepository[G] = {
      var updated = super.init(gamma, history)

      registerImpl(history, new Eval, new FunctionMethod("eval", Types.Double)).foreach(comb =>
        updated = updated.addCombinator(comb)
      )

      updated
    }

  /**
    * Code generator for reproducing the structure of the covariant invocation for eval.
    */
  var evalGenerators:CodeGeneratorRegistry[Seq[Statement]] = CodeGeneratorRegistry.merge[Seq[Statement]](
    CodeGeneratorRegistry[Seq[Statement], Lit] {
      case (_:CodeGeneratorRegistry[Seq[Statement]], _:Lit) =>
        Java(s"""return value();""").statements()
    },

    CodeGeneratorRegistry[Seq[Statement], Add] {
      case (_:CodeGeneratorRegistry[Seq[Statement]], _:Add) =>
        Java(s"""return left().eval() + right().eval();""").statements()
    },
  )

}
