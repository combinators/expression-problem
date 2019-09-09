package org.combinators.ep.language.java

import org.combinators.ep.generator.AbstractSyntax

trait Syntax extends AbstractSyntax {
  type CompilationUnit = com.github.javaparser.ast.CompilationUnit
  type Import = com.github.javaparser.ast.ImportDeclaration
  type Expression = com.github.javaparser.ast.expr.Expression
  type Type = com.github.javaparser.ast.`type`.Type
  type Statement = com.github.javaparser.ast.stmt.Statement
  type UnitTest = com.github.javaparser.ast.body.ClassOrInterfaceDeclaration
}

object Syntax {
  val default: Syntax = new Syntax {}
}
