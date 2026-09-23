package org.combinators.ep.language.inbetween.any    /*DI:LI:AI*/

import org.combinators.cogen.AbstractSyntax as AS

trait AbstractSyntax extends AS {
  val ast: AnyAST
  
  type CompilationUnit = ast.any.CompilationUnit
  type Import = ast.any.Import
  type Expression = ast.any.Expression
  type Type = ast.any.Type
  type Statement = ast.any.Statement
  type UnitTest = Unit //TODO: ast.any.UnitTest
  type Name = ast.any.Name
}

object AbstractSyntax {
  type WithAST[AST <: AnyAST] = org.combinators.ep.language.inbetween.any.AbstractSyntax { val ast: AST }
  def apply[AST <: AnyAST](_ast: AST): AbstractSyntax.WithAST[_ast.type] = {
    class AS(override val ast: _ast.type = _ast) extends AbstractSyntax {}
    new AS()
  } 
}
