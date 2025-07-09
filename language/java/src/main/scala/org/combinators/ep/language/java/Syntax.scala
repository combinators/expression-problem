package org.combinators.ep.language.java     /*DI:LD:AI*/

import org.combinators.cogen.AbstractSyntax
import cats.data.State
import cats._

trait Syntax extends AbstractSyntax {
  type CompilationUnit = com.github.javaparser.ast.CompilationUnit
  type Import = com.github.javaparser.ast.ImportDeclaration
  type Expression = com.github.javaparser.ast.expr.Expression
  type Type = com.github.javaparser.ast.`type`.Type
  type Statement = com.github.javaparser.ast.stmt.Statement
  type UnitTest = com.github.javaparser.ast.body.ClassOrInterfaceDeclaration
  type Name = Syntax.MangledName
}

object Syntax {
  case class MangledName(original: String, mangled: String) {
    def toAST: com.github.javaparser.ast.expr.SimpleName = {
      new com.github.javaparser.ast.expr.SimpleName(
        if (original != mangled) s"/*${original.replaceAll("\\*/", "* /")}*/$mangled" else mangled
      )
    }

    override def toString: String = toAST.toString
  }
  object MangledName {
    def fromAST(ast: com.github.javaparser.ast.expr.SimpleName): MangledName = {
      val mangled = ast.toString
      val original = ast.getComment.map[String](_.getContent).orElseGet(() => mangled)
      MangledName(original, mangled)
    }
  }

  val default: Syntax = new Syntax {}
}
