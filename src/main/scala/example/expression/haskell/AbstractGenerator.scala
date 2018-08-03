package example.expression.haskell     /*DI:LD:AI*/

import example.expression.generator.LanguageIndependentGenerator

/**
  * Any Haskell EP approach can extend this Generator
  */
trait AbstractGenerator extends LanguageIndependentGenerator {

  type CompilationUnit = HaskellWithPath
  type Type = HaskellType
  type Expression = Haskell
  type Statement = Haskell

  /** Concatenate attributes by name in order */
  def standardArgs(exp:domain.Atomic, suffix:String = "") : Haskell = {
    Haskell(exp.attributes.map(att => att.name + suffix).mkString(" "))
  }

  /** If any new imports are needed for an operation, just extend here. */
  def addedImports(op:domain.Operation):Seq[Haskell] = Seq.empty

}
