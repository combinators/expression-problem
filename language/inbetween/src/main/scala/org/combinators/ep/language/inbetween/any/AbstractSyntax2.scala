package org.combinators.ep.language.inbetween.any

/*DI:LI:AI*/

import org.combinators.cogen.AbstractSyntax as AS

trait AbstractSyntax2(val ast: AnyAST) extends AS {
  type CompilationUnit = ast.any.CompilationUnit
  type Import = ast.any.Import
  type Expression = ast.any.Expression
  type Type = ast.any.Type
  type Statement = ast.any.Statement
  type UnitTest = Unit //TODO: ast.any.UnitTest
  type Name = ast.any.Name
}

object AbstractSyntax2 {
  type AbstractSyntax[AST <: AnyAST] = AbstractSyntax2 { val ast: AST }
}
