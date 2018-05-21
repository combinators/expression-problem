package example.expression.covariant.e2

import com.github.javaparser.ast.stmt.Statement
import example.expression.covariant.{Registry, SemanticTypes}
import example.expression.j.Operators
import example.expression.{Base, ExpressionDomain}
import expression.data._
import expression.extensions.{PrettyP, Sub}
import expression.history.History
import org.combinators.cls.interpreter.ReflectedRepository
import org.combinators.templating.twirl.Java
import shared.compilation.CodeGeneratorRegistry

trait Model extends Base with Registry with Operators with SemanticTypes {

  /**
    * Code generator for reproducing the structure of the covariant invocation for prettyP.
    */
  var prettyPGenerators:CodeGeneratorRegistry[Seq[Statement]] = CodeGeneratorRegistry.merge[Seq[Statement]](
    CodeGeneratorRegistry[Seq[Statement], Lit] {
      case (_:CodeGeneratorRegistry[Seq[Statement]], _:Lit) =>
        Java(s"""return "" + value();""").statements()
    },

    CodeGeneratorRegistry[Seq[Statement], Add] {
      case (_:CodeGeneratorRegistry[Seq[Statement]], _:Add) =>
        Java(s"""return "(" + left().print() + "+" + right().print() + ")";""").statements()
    },

    CodeGeneratorRegistry[Seq[Statement], Sub] {
      case (_:CodeGeneratorRegistry[Seq[Statement]], _:Sub) =>
        Java(s"""return "(" + left().print() + "-" + right().print() + ")";""").statements()
    },
  )

     /** Add dynamic combinators as needed. */
    override def init[G <: ExpressionDomain](gamma: ReflectedRepository[G], history: History): ReflectedRepository[G] = {
      var updated = super.init(gamma, history)

      registerExtension(history, new PrettyP, prettyPGenerators).foreach(comb =>
        updated = updated.addCombinator(comb)
      )

      updated
    }


}
