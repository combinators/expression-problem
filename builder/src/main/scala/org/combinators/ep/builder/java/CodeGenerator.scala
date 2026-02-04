package org.combinators.ep.builder.java

import com.github.javaparser.ast.PackageDeclaration
import org.combinators.cogen.Command
import org.combinators.ep.builder.java.paradigm.ffi.Trees
import org.combinators.ep.language.java.{Config, CtorCtxt, MethodBodyCtxt}
import org.combinators.ep.language.java.paradigm.ObjectOriented

class CodeGenerator(config: Config) extends org.combinators.ep.language.java.CodeGenerator(config) {

  val treesInMethod = {
    Trees[MethodBodyCtxt, paradigm.type, ObjectOriented](
      paradigm,
      paradigm.methodBodyCapabilities.canAddImportInMethodBody
    )(ooParadigm)
  }

  val treesInConstructor = {
    Trees[CtorCtxt, paradigm.type, ObjectOriented](
      paradigm,
      ooParadigm.constructorCapabilities.canAddImportInConstructor
    )(ooParadigm)
  }
}

object CodeGenerator {
  def apply(config: Config = org.combinators.ep.language.java.CodeGenerator.defaultConfig): CodeGenerator =
    new CodeGenerator(config)
}
