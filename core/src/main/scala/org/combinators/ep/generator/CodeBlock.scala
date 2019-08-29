package org.combinators.ep.generator

import cats.free.Free
import cats.free.Free.liftF

trait CodeBlock[-S <: AbstractSyntax, R]
case class AddImport[Imp](imp: Imp) extends CodeBlock[CodeBlock.SyntaxWithImport[Imp], Unit]
case class AddContext[Stmt](stmts: Seq[Stmt]) extends CodeBlock[CodeBlock.SyntaxWithStatement[Stmt], Unit]

object CodeBlock {
  type SyntaxWithImport[I] = AbstractSyntax { type Import = I }
  type SyntaxWithStatement[S] = AbstractSyntax { type Statement = S }

  type Generator[S <: AbstractSyntax, A] = Free[CodeBlock[S, *], A]

  def addImport[Imp, S <: SyntaxWithImport[Imp]](imp: Imp): Generator[S, Unit] =
    liftF[CodeBlock[S, *], Unit](AddImport(imp))

  def addContext[Stmt, S <: SyntaxWithStatement[Stmt]](stmts: Seq[Stmt]): Generator[S, Unit] =
    liftF[CodeBlock[S, *], Unit](AddContext(stmts))

}