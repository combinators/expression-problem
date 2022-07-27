package org.combinators.ep.language.scala   /*DI:LD:AI*/

import scala.meta.dialects.Scala3._
import scala.meta._

import org.combinators.ep.generator.AbstractSyntax

trait Syntax extends AbstractSyntax{
  type CompilationUnit = Source
  type Import = scala.meta.Import
  type Expression = Term
  type Type = scala.meta.Type
  type Statement = Stat
  type UnitTest = Defn.Class
  type Name = Syntax.MangledName
}

object Syntax {
  case class MangledName(original: String, mangled: String) {
    def toAST: scala.meta.Name = {
      val decorated =
        if (original != mangled) s"/*${original.replaceAll("\\*/", "* /")}*/$mangled" else mangled
      decorated.parse[Term].get.asInstanceOf[Term.Name]
    }

    override def toString: String = toAST.toString
  }
  object MangledName {
    def fromAST(ast: scala.meta.Name): MangledName = {
      val mangled = ast.value
      val original = ast.tokens.collectFirst {
          case c: tokens.Token.Comment => c.value
        }.getOrElse(mangled)
      MangledName(original, mangled)
    }
  }

  val default: Syntax = new Syntax {}
}
