package org.combinators.ep.generator

/**
 * Each Language Independent interface needs this.
 */
abstract class Binding {
  /** @group lang */
  type CompilationUnit

  /** @group lang */
  type Type

  /** @group lang */
  type Expression

  /** @group lang */
  type Statement
}
