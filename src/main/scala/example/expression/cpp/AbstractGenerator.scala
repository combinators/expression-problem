package example.expression.cpp        /*DI:LD:AI*/

import example.expression.generator.LanguageIndependentGenerator

/**
  * Any Haskell EP approach can extend this Generator
  */
trait AbstractGenerator extends LanguageIndependentGenerator {

  type CompilationUnit = CPPFile
  type Type = CPPType
  type Expression = CPPElement
  type Statement = CPPElement

}
