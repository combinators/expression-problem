package example.expression.cpp.e2

import example.expression.cpp.{CPPMethod, HasCPPCodeGenerator, HasCPPTestCaseGenerator}
import expression.Operation
import expression.data._
import expression.extensions.{PrettyP, Sub}
import expression.instances.UnitTest
import shared.compilation.CodeGeneratorRegistry

/**
 * Designed knowing this comes after E1, and thus must account for Lit, Add (E0) and Sub (E1)
 */
trait Model extends HasCPPCodeGenerator with HasCPPTestCaseGenerator {

  abstract override def codeGenerator:CodeGeneratorRegistry[CodeGeneratorRegistry[CPPMethod]] = {
    val oldGenerator = super.codeGenerator

    // it is critical that the new changes are merged before old ones
    CodeGeneratorRegistry.merge(

      CodeGeneratorRegistry[CodeGeneratorRegistry[CPPMethod], PrettyP] {

        case (_, pp:PrettyP) =>
          CodeGeneratorRegistry.merge(
            oldGenerator(pp).getOrElse(CodeGeneratorRegistry[CPPMethod]),

            CodeGeneratorRegistry[CPPMethod, Lit] {
              case (_, exp:Lit) =>
                val name = exp.getClass.getSimpleName
                new CPPMethod("void", s"Visit$name", s"(const $name* e)",
                  s"""|std::ostringstream ss;
                      |double val = *e->getValue();
                      |int ival = (int) val;
                      |ss << *e->getValue();
                      |if (val == ival) { ss << ".0"; }  // add trailing .0 for int-value doubles
                      |value_map_[e] = ss.str();""".stripMargin.split("\n")
                )
            },

            CodeGeneratorRegistry[CPPMethod, Add] {
              case (ppGen, exp:Add) =>
                val name = exp.getClass.getSimpleName
                new CPPMethod("void", s"Visit$name", s"(const $name* e)",
                  Seq(s"""value_map_[e] = "(" + value_map_[e->getLeft()] + "+" + value_map_[e->getRight()] + ")";""")
                )
            },

            CodeGeneratorRegistry[CPPMethod, Sub] {
              case (ppGen, exp:Sub) =>
                val name = exp.getClass.getSimpleName
                new CPPMethod("void", s"Visit$name", s"(const $name* e)",
                  Seq(s"""value_map_[e] = "(" + value_map_[e->getLeft()] + "-" + value_map_[e->getRight()] + ")";""")
                )
            }
          )

        case (_,_) => CodeGeneratorRegistry[CPPMethod]
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
  abstract override def testCaseGenerator(op:Operation, identifier:String, tc: UnitTest) : Seq[String] = {

    if (op.equals(new PrettyP)) {
      val num: Int = nextTestNumber()
      s"""|  String result$num = (String) ${identifier.toString}.accept(new PrettyP());
          |  assertEquals("${tc.expected.toString}", result$num);
          |""".stripMargin.split("\n")
    } else {
      super.testCaseGenerator(op, identifier, tc)
    }
  }
}
