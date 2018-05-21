package example.expression.covariant.e3

import com.github.javaparser.ast.stmt.Statement
import example.expression.covariant.{Registry, SemanticTypes}
import example.expression.j.Operators
import example.expression.{Base, ExpressionDomain}
import expression.extensions._
import expression.history.History
import org.combinators.cls.interpreter.ReflectedRepository
import org.combinators.templating.twirl.Java
import shared.compilation.CodeGeneratorRegistry

trait Model extends Base with Registry with Operators with SemanticTypes {

  // will be made present in e0, so we can assume this is there
  var evalGenerators:CodeGeneratorRegistry[Seq[Statement]]
  var prettyPGenerators:CodeGeneratorRegistry[Seq[Statement]]


  /** Add dynamic combinators as needed. */
    override def init[G <: ExpressionDomain](gamma: ReflectedRepository[G], history: History): ReflectedRepository[G] = {
      var updated = super.init(gamma, history)

      updated
    }

  /**
    * Code generator for Mult, Divd, Neg with eval
    */
  evalGenerators = evalGenerators.merge(CodeGeneratorRegistry[Seq[Statement], Mult] {
    case (_:CodeGeneratorRegistry[Seq[Statement]], _:Mult) =>
      Java(s"""return left().eval() * right().eval();""").statements()
  })

  evalGenerators = evalGenerators.merge(CodeGeneratorRegistry[Seq[Statement], Divd] {
    case (_:CodeGeneratorRegistry[Seq[Statement]], _:Divd) =>
      Java(s"""return left().eval() / right().eval();""").statements()
  })

  evalGenerators = evalGenerators.merge(CodeGeneratorRegistry[Seq[Statement], Neg] {
    case (_:CodeGeneratorRegistry[Seq[Statement]], _:Neg) =>
      Java(s"""return -exp().eval();""").statements()
  })

  /**
    * Code for prettyP extensions
    */
  prettyPGenerators = prettyPGenerators.merge(CodeGeneratorRegistry[Seq[Statement], Mult] {
    case (_:CodeGeneratorRegistry[Seq[Statement]], _:Mult) =>
      Java(s"""return "(" + left().print() + "*" + right().print() + ")";""").statements()
  })

  prettyPGenerators = prettyPGenerators.merge(CodeGeneratorRegistry[Seq[Statement], Divd] {
    case (_:CodeGeneratorRegistry[Seq[Statement]], _:Divd) =>
      Java(s"""return "(" + left().print() + "/" + right().print() + ")";""").statements()
  })

  prettyPGenerators = prettyPGenerators.merge(CodeGeneratorRegistry[Seq[Statement], Neg] {
    case (_:CodeGeneratorRegistry[Seq[Statement]], _:Neg) =>
      Java(s"""return "-" + exp().print();""").statements()
  })
}
