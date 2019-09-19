package org.combinators.ep.language.java

import org.combinators.ep.generator.{AbstractSyntax, FreshNameProvider}
import cats.data.State
import cats._

trait Syntax extends AbstractSyntax {
  type CompilationUnit = com.github.javaparser.ast.CompilationUnit
  type Import = com.github.javaparser.ast.ImportDeclaration
  type Expression = State[FreshNameProvider[Syntax.MangledName], com.github.javaparser.ast.expr.Expression]
  type Type = State[FreshNameProvider[Syntax.MangledName], com.github.javaparser.ast.`type`.Type]
  type Statement = State[FreshNameProvider[Syntax.MangledName], com.github.javaparser.ast.stmt.Statement]
  type UnitTest = State[FreshNameProvider[Syntax.MangledName], com.github.javaparser.ast.body.ClassOrInterfaceDeclaration]
  type Name = State[FreshNameProvider[Syntax.MangledName], Syntax.MangledName]
}

object Syntax {
  case class MangledName(original: String, mangled: String) {
    def toAST: com.github.javaparser.ast.expr.SimpleName = {
      new com.github.javaparser.ast.expr.SimpleName(
        if (original != mangled) s"/* $original */$mangled" else mangled
      )
    }
  }
  val default: Syntax = new Syntax {}
}
