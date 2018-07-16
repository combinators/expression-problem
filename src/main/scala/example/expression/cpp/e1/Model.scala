package example.expression.cpp.e1

import example.expression.cpp.{CPPMethod, HasCPPCodeGenerator}
import expression.data.Eval
import expression.extensions.Sub
import shared.compilation.CodeGeneratorRegistry

trait Model extends HasCPPCodeGenerator  {

   abstract override def codeGenerator:CodeGeneratorRegistry[CodeGeneratorRegistry[CPPMethod]] = {
     val oldGenerator = super.codeGenerator

     // it is critical that the new changes are merged before old ones
     CodeGeneratorRegistry.merge(
       CodeGeneratorRegistry[CodeGeneratorRegistry[CPPMethod], Eval] {
         case (_, eval:Eval) =>

           val oldGen:CodeGeneratorRegistry[CPPMethod] = oldGenerator(eval).getOrElse(CodeGeneratorRegistry[CPPMethod])

           oldGen.merge(CodeGeneratorRegistry[CPPMethod, Sub] {
             case (_:CodeGeneratorRegistry[CPPMethod], exp:Sub) =>
               val name = exp.getClass.getSimpleName
               new CPPMethod("void", s"Visit$name", s"(const $name* e)",
                 "value_map_[e] = value_map_[e->getLeft()] - value_map_[e->getRight()];")
           }
           )
       },

       oldGenerator
    )

  }
}
