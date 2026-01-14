package org.combinators.ep.language.inbetween.any

/*DI:LI:AI*/

import org.combinators.cogen.AbstractSyntax as AS

trait AbstractSyntax[A](val ast: AnyAST & A) extends AS {
  type CompilationUnit = ast.any.CompilationUnit
  type Import = ast.any.Import
  type Expression = ast.any.Expression
  type Type = ast.any.Type
  type Statement = ast.any.Statement
  type UnitTest = Unit //TODO: ast.any.UnitTest
  type Name = ast.any.Name
}

object AbstractSyntax {
  type AbstractSyntax[AST <: AnyAST] = org.combinators.ep.language.inbetween.any.AbstractSyntax[AST] { }
  def apply[AST <: AnyAST](ast: AST): AbstractSyntax[ast.type] = new AbstractSyntax[ast.type](ast) {}
}
