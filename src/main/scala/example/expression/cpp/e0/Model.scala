package example.expression.cpp.e0

import com.github.javaparser.ast.expr.SimpleName
import com.github.javaparser.ast.stmt.Statement
import expression.Operation
import expression.data.{Add, Eval, Lit}
import expression.instances.UnitTest
import org.combinators.templating.twirl.Java
import shared.compilation.{CodeGeneratorRegistry, HasCodeGenerator, HasTestCaseGenerator}

/**
  * Each trait is stand alone, and woven into the final repository.
  */
trait Model extends HasCodeGenerator with HasTestCaseGenerator {

  // starting point. Eval is first one
  def codeGenerator: CodeGeneratorRegistry[CodeGeneratorRegistry[Seq[Statement]]] = {

    // First one is defined here
    CodeGeneratorRegistry[CodeGeneratorRegistry[Seq[Statement]], Eval] {

      case (_, eval: Eval) =>
        CodeGeneratorRegistry.merge(

          CodeGeneratorRegistry[Seq[Statement], Lit] {
            case (_, dataty: Lit) =>
              Java(s"""return e.getValue();""").statements()
          },

          CodeGeneratorRegistry[Seq[Statement], Add] {
            case (_, dataty: Add) =>
              Java(s"""return e.getLeft().accept(this) + e.getRight().accept(this);""").statements()
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
      val num: Int = nextTestNumber()

      Java(
        s"""|  Double result$num = (Double) ${identifier.toString}.accept(new Eval());
            |  assertEquals(${tc.expected.toString}, result$num.doubleValue());
            |""".stripMargin).statements()
    } else {
      super.testCaseGenerator(op, identifier, tc)
    }
  }
}
