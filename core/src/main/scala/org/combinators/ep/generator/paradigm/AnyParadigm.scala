package org.combinators.ep.generator.paradigm

import org.combinators.ep.generator.{AbstractSyntax, AddBlockDefinitions, AddImport, Understands}

abstract class AnyParadigm[S <: AbstractSyntax](val syntax: S) {
  import syntax._

  type CompilationUnitContext
  type MethodBodyContext

  implicit val canAddImportInCompilationUnit: Understands[CompilationUnitContext, AddImport[Import]]
  implicit val canAddImportInMethodBody: Understands[MethodBodyContext, AddImport[Import]]
  implicit val canAddBlockDefinitionsInMethodBody: Understands[MethodBodyContext, AddBlockDefinitions[Statement]]
}

