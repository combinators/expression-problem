package org.combinators.ep.language.scala

import scala.meta._

import org.combinators.ep.generator.AbstractSyntax

trait Syntax extends AbstractSyntax{
  type CompilationUnit = Source
  type Import = scala.meta.Import
  type Expression = Term
  type Type = scala.meta.Type
  type Statement = Term
  type UnitTest = Defn.Class
  type Name = scala.meta.Name
}
