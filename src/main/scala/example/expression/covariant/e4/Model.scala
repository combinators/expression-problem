package example.expression.covariant.e4

import com.github.javaparser.ast.stmt.Statement
import example.expression.covariant.{Registry, SemanticTypes}
import example.expression.j.Operators
import example.expression.{Base, ExpressionDomain}
import expression.data.{Add, Lit}
import expression.extensions._
import expression.history.History
import expression.operations.SimplifyExpr
import org.combinators.cls.interpreter.ReflectedRepository
import org.combinators.templating.twirl.Java
import shared.compilation.CodeGeneratorRegistry

trait Model extends Base with Registry with Operators with SemanticTypes {

    var simplifyGenerators:CodeGeneratorRegistry[Seq[Statement]]= CodeGeneratorRegistry.merge()

    /** Add dynamic combinators as needed. */
    override def init[G <: ExpressionDomain](gamma: ReflectedRepository[G], history: History): ReflectedRepository[G] = {
      var updated = super.init(gamma, history)


      registerExtension(history, new Collect, collectLitGenerators).foreach(comb =>
        updated = updated.addCombinator(comb)
      )

      // Get class that contains just PrettyP and SimplifyExp
      val subTypes:String = List(new PrettyP().getClass.getSimpleName,
        new SimplifyExpr().getClass.getSimpleName)
        .sortWith(_ < _)
        .mkString("")

      simplifyGenerators = new SimplifyCodeGenerators(subTypes).simplifyGenerators

      registerExtension(history, new SimplifyExpr, simplifyGenerators).foreach(comb =>
        updated = updated.addCombinator(comb)
      )

      updated
    }

  /**
    * Code generator for reproducing the structure of the covariant pattern invocation for collectLit.
    * Should be simplified since the repeated code exists
    */
  var collectLitGenerators:CodeGeneratorRegistry[Seq[Statement]] = CodeGeneratorRegistry.merge[Seq[Statement]](
    CodeGeneratorRegistry[Seq[Statement], Lit] {
      case (_:CodeGeneratorRegistry[Seq[Statement]], _:Lit) =>
        Java("""|java.util.List<Double> list = new java.util.ArrayList<Double>();
                |list.add(value());
                |return list;
                |""".stripMargin).statements()
    })

  val listStatements:Seq[Statement] = Java("""|java.util.List<Double> list = new java.util.ArrayList<Double>();
                                              |list.addAll(left().collectList());
                                              |list.addAll(right().collectList());
                                              |return list;
                                              |""".stripMargin).statements()

  collectLitGenerators = collectLitGenerators.merge(CodeGeneratorRegistry[Seq[Statement], Add] {
    case (_:CodeGeneratorRegistry[Seq[Statement]], _:Add) => listStatements })
  collectLitGenerators = collectLitGenerators.merge(CodeGeneratorRegistry[Seq[Statement], Sub] {
    case (_:CodeGeneratorRegistry[Seq[Statement]], _:Sub) => listStatements })
  collectLitGenerators = collectLitGenerators.merge(CodeGeneratorRegistry[Seq[Statement], Mult] {
    case (_:CodeGeneratorRegistry[Seq[Statement]], _:Mult) => listStatements })
  collectLitGenerators = collectLitGenerators.merge(CodeGeneratorRegistry[Seq[Statement], Divd] {
    case (_:CodeGeneratorRegistry[Seq[Statement]], _:Divd) => listStatements })
  collectLitGenerators = collectLitGenerators.merge(CodeGeneratorRegistry[Seq[Statement], Neg] {
    case (_:CodeGeneratorRegistry[Seq[Statement]], _:Neg) => Java("""|java.util.List<Double> list = new java.util.ArrayList<Double>();
                                                                     |list.addAll(exp().collectList());
                                                                     |return list;
                                                                     |""".stripMargin).statements() })


}
