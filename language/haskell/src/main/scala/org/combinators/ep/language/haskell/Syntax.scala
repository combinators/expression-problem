package org.combinators.ep.language.haskell     /*DI:LD:AI*/

import org.combinators.ep.generator.AbstractSyntax

import java.nio.charset.StandardCharsets

trait Syntax extends AbstractSyntax {
  type CompilationUnit = ast.CompilationUnit
  type Import = ast.Import
  type Expression = ast.Expression
  type Type = ast.Type
  type Statement = ast.Expression
  type UnitTest = ast.CompilationUnit
  type Name = Syntax.MangledName
}

object Syntax {
  case class MangledName(original: String, mangled: String) {
    def toAST: ast.SimpleName = ast.SimpleName(mangled)
    override def toString: String = toAST.toString
  }
  object MangledName {
    def fromAST(n: ast.SimpleName): MangledName = {
      if (n.name.startsWith("mangled_")) {
        val bytes: Array[Byte] =
          n.name.drop("mangled_".length).split("_").map(s => Integer.valueOf(s).byteValue())
        MangledName(new String(bytes, StandardCharsets.UTF_8), n.name)
      } else {
        MangledName(n.name, n.name)
      }
    }
  }

  trait QualifiedMangledName {
    def toAST: ast.Name
  }
  implicit def toQualifiedMangledName(names: Seq[MangledName]): QualifiedMangledName = new QualifiedMangledName {
    def toAST: ast.Name = ast.Name(names.map(_.toAST.name):_*)
  }

  val default: Syntax = new Syntax {}
}
