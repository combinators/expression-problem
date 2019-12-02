package org.combinators.ep.generator

/** Provides type definitions for abstract syntax tree components. */
trait AbstractSyntax {
  /** Represents a single file of source code. */
  type CompilationUnit

  /** Represent an import declaration to pull in library code. */
  type Import

  /** Represents a single expression. */
  type Expression

  /** Represents a type annotation or declaration. */
  type Type

  /** Represents a single statement, e.g. a line of imperative source code. */
  type Statement

  /** Represents a single test case. */
  type UnitTest

  /** Represents a name. */
  type Name
}
