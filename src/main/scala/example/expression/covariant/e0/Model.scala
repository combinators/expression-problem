package example.expression.covariant.e0

import com.github.javaparser.ast.expr.SimpleName
import com.github.javaparser.ast.stmt.Statement
import expression.Operation
import expression.data._
import expression.instances.UnitTest
import org.combinators.templating.twirl.Java
import shared.compilation.{CodeGeneratorRegistry, HasCodeGenerator, HasTestCaseGenerator}

trait Model extends HasCodeGenerator with HasTestCaseGenerator {

//    /** Add dynamic combinators as needed. */
//    override def init[G <: ExpressionDomain](gamma: ReflectedRepository[G], history: History): ReflectedRepository[G] = {
//      var updated = super.init(gamma, history)
//
//      registerImpl(history, new Eval, new FunctionMethod("eval", Types.Double)).foreach(comb =>
//        updated = updated.addCombinator(comb)
//      )
//
//      updated
//    }

  // starting point. Eval is first one
  def codeGenerator: CodeGeneratorRegistry[CodeGeneratorRegistry[Seq[Statement]]] = {

    // First one is defined here
    CodeGeneratorRegistry[CodeGeneratorRegistry[Seq[Statement]], Eval] {

      case (_, eval: Eval) =>
        CodeGeneratorRegistry.merge(

          CodeGeneratorRegistry[Seq[Statement], Lit] {
            case (_, dataty: Lit) =>
              Java(s"""return value();""").statements()
          },

          CodeGeneratorRegistry[Seq[Statement], Add] {
            case (_, dataty: Add) =>
              Java(s"""return left().eval() + right().eval();""").statements()
          }
        )
    }
  }


  /**
    * Create test case code for eval where the expression "identifier"  has already been constructed
    * and the test case is UnitTest, which has its own expectations.
    *
    * Forms chain of responsibility
    */
  abstract override def testCaseGenerator(op:Operation, identifier:SimpleName, tc: UnitTest) : Seq[Statement] = {

    // only handle Eval
    if (op.equals(new Eval)) {
      Java(s"""|  assertEquals(${tc.expected.toString}, ${identifier.toString}.eval());
               |""".stripMargin).statements()
    } else {
      super.testCaseGenerator(op, identifier, tc)
    }
  }

}
