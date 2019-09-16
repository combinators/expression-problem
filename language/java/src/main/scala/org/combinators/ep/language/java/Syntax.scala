package org.combinators.ep.language.java

import org.combinators.ep.generator.{AbstractSyntax, FreshNameProvider}
import cats.data.State
import cats._

trait Syntax extends AbstractSyntax {
  type CompilationUnit = com.github.javaparser.ast.CompilationUnit
  type Import = com.github.javaparser.ast.ImportDeclaration
  type Expression = State[FreshNameProvider, com.github.javaparser.ast.expr.Expression]
  type Type = State[FreshNameProvider, com.github.javaparser.ast.`type`.Type]
  type Statement = State[FreshNameProvider, com.github.javaparser.ast.stmt.Statement]
  type UnitTest = State[FreshNameProvider, com.github.javaparser.ast.body.ClassOrInterfaceDeclaration]
}

object Syntax {
  val default: Syntax = new Syntax {}
}
