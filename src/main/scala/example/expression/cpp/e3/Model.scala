package example.expression.cpp.e3

import example.expression.cpp.{CPPMethod, HasCPPCodeGenerator}
import expression.data._
import expression.extensions._
import shared.compilation.CodeGeneratorRegistry

/**
 * Designed knowing this comes after J1, and thus must account for Lit, Add (J0) and Sub (J1)
 */
trait Model extends HasCPPCodeGenerator {

  abstract override def codeGenerator:CodeGeneratorRegistry[CodeGeneratorRegistry[CPPMethod]] = {
    val oldGenerator = super.codeGenerator

    // it is critical that the new changes are merged before old ones
    CodeGeneratorRegistry.merge(

      CodeGeneratorRegistry[CodeGeneratorRegistry[CPPMethod], Eval] {

        case (_, e:Eval) =>
          CodeGeneratorRegistry.merge(
            oldGenerator(e).getOrElse(CodeGeneratorRegistry[CPPMethod]),

            CodeGeneratorRegistry[CPPMethod, Divd] {
              case (_, exp:Divd) =>
                val name = exp.getClass.getSimpleName
                new CPPMethod("void", s"Visit$name", s"(const $name* e)",
                  "value_map_[e] = value_map_[e->getLeft()] / value_map_[e->getRight()];")
            },

            CodeGeneratorRegistry[CPPMethod, Mult] {
              case (_, exp:Mult) =>
                val name = exp.getClass.getSimpleName
                new CPPMethod("void", s"Visit$name", s"(const $name* e)",
                  "value_map_[e] = value_map_[e->getLeft()] * value_map_[e->getRight()];")
            },

            CodeGeneratorRegistry[CPPMethod, Neg] {
              case (_, exp:Neg) =>
                val name = exp.getClass.getSimpleName
                new CPPMethod("void", s"Visit$name", s"(const $name* e)",
                  "value_map_[e] = - value_map_[e->getExp()];")
            }
          )

        case (_,_) => CodeGeneratorRegistry[CPPMethod]
      },

      CodeGeneratorRegistry[CodeGeneratorRegistry[CPPMethod], PrettyP] {

        case (_, pp:PrettyP) =>
          CodeGeneratorRegistry.merge(
            oldGenerator(pp).getOrElse(CodeGeneratorRegistry[CPPMethod]),

            CodeGeneratorRegistry[CPPMethod, Divd] {
              case (ppGen, exp:Divd) =>
                val name = exp.getClass.getSimpleName
                new CPPMethod("void", s"Visit$name", s"(const $name* e)",
                  Seq(s"""value_map_[e] = "(" + value_map_[e->getLeft()] + "/" + value_map_[e->getRight()] + ")";""")
                )
            },

            CodeGeneratorRegistry[CPPMethod, Mult] {
              case (ppGen, exp:Mult) =>
                val name = exp.getClass.getSimpleName
                new CPPMethod("void", s"Visit$name", s"(const $name* e)",
                  Seq(s"""value_map_[e] = "(" + value_map_[e->getLeft()] + "*" + value_map_[e->getRight()] + ")";""")
                )
            },

            CodeGeneratorRegistry[CPPMethod, Neg] {
              case (ppGen, exp:Neg) =>
                val name = exp.getClass.getSimpleName
                new CPPMethod("void", s"Visit$name", s"(const $name* e)",
                  Seq(s"""value_map_[e] = "(" + value_map_[e->getExp()] + ")";""")
                )
            }
          )

        case (_,_) => CodeGeneratorRegistry[CPPMethod]
      },

      oldGenerator
    )
  }

}
