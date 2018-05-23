package example.expression.visitor.e2

import com.github.javaparser.ast.expr.SimpleName
import com.github.javaparser.ast.stmt.Statement
import expression.{Exp, Operation}
import expression.data._
import expression.extensions.{PrettyP, Sub}
import expression.instances.UnitTest
import org.combinators.templating.twirl.Java
import shared.compilation.{CodeGeneratorRegistry, HasCodeGenerator, HasTestCaseGenerator}

/**
 * Designed knowing this comes after E1, and thus must account for Lit, Add (E0) and Sub (E1)
 */
trait Model extends HasCodeGenerator with HasTestCaseGenerator {

  abstract override def codeGenerator:CodeGeneratorRegistry[CodeGeneratorRegistry[Seq[Statement]]] = {
    val oldGenerator = super.codeGenerator

    // it is critical that the new changes are merged before old ones
    CodeGeneratorRegistry.merge(

      CodeGeneratorRegistry[CodeGeneratorRegistry[Seq[Statement]], PrettyP] {

        case (_, pp:PrettyP) =>
          CodeGeneratorRegistry.merge(
            oldGenerator(pp).getOrElse(CodeGeneratorRegistry[Seq[Statement]]),

            CodeGeneratorRegistry[Seq[Statement], Lit] {
              case (_, dataty:Lit) =>
                Java(s"""return "" + e.getValue() + ""; """).statements()
            },

            CodeGeneratorRegistry[Seq[Statement], Add] {
              case (ppGen, dataty:Add) =>
                Java(s"""return "(" + e.getLeft().accept(this) + "+" + e.getRight().accept(this) + ")"; """).statements()
            },
            CodeGeneratorRegistry[Seq[Statement], Sub] {
              case (ppGen, dataTY:Sub) =>
                Java(s"""return "(" + e.getLeft().accept(this) + "-" + e.getRight().accept(this) + ")"; """).statements()
            }
          )

        case (_,_) => CodeGeneratorRegistry[Seq[Statement]]
      },

      oldGenerator
    )
  }

  /**
    * Create test case code for eval where the expression "identifier"  has already been constructed
    * and the test case is UnitTest, which has its own expectations.
    *
    * Forms chain of responsibility
    */
  abstract override def testCaseGenerator(op:Operation, identifier:SimpleName, tc: UnitTest) : Seq[Statement] = {

    if (op.equals(new PrettyP)) {
      val num: Int = nextTestNumber()
      Java(s"""|  String result$num = (String) ${identifier.toString}.accept(new PrettyP());
               |  assertEquals("${tc.expected.toString}", result$num);
               |""".stripMargin).statements()

    } else {
      super.testCaseGenerator(op, identifier, tc)
    }
  }

}
