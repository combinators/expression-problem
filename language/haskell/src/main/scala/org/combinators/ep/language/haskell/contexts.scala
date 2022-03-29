package org.combinators.ep.language.haskell     /*DI:LD:AI*/

import org.combinators.ep.generator.FreshNameProvider
import org.combinators.ep.language.haskell.ast.{CompilationUnit, FunDecl, Name, TypeDecl}



case class ProjectCtxt(
  resolver: ContextSpecificResolver,
  units: Seq[CompilationUnit],
  testUnits: Seq[CompilationUnit],
  extraDependencies: Seq[String]
)
case class CompilationUnitCtxt(
  resolver: ContextSpecificResolver,
  freshNameProvider: FreshNameProvider[Syntax.default.Name],
  unit: CompilationUnit,
  isTest: Boolean
)
case class TypeCtxt(
  resolver: ContextSpecificResolver,
  freshNameProvider: FreshNameProvider[Syntax.default.Name],
  tpe: Name => TypeDecl,
  extraImports: Seq[Name]
)

case class TestCtxt(
  resolver: ContextSpecificResolver,
  freshNameProvider: FreshNameProvider[Syntax.default.Name],
  extraImports: Seq[Name]
)
case class MethodBodyCtxt(
  resolver: ContextSpecificResolver,
  freshNameProvider: FreshNameProvider[Syntax.default.Name],
  extraImports: Seq[Name],
  method: Name => FunDecl
)