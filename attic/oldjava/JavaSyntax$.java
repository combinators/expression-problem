package org.combinators.ep.language.java;

/** Provides abstract syntax definitions for Java using the abstractions from [[https://javaparser.org java parser]]. */
object JavaSyntax extends AbstractSyntax {
  type CompilationUnit = com.github.javaparser.ast.CompilationUnit
  type Type = com.github.javaparser.ast.`type`.Type
  type Expression = com.github.javaparser.ast.expr.Expression
  type Statement = com.github.javaparser.ast.stmt.Statement
  type UnitTest = com.github.javaparser.ast.body.MethodDeclaration
}
