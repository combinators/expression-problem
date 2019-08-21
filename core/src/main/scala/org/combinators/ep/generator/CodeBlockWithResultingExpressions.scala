package org.combinators.ep.generator

/**
  * Represents a block of code with result values.
  *
  * This separates out the resulting expressions from the block of statements (and they don't have to show up
  * in the code block).
  */
trait CodeBlockWithResultingExpressions[Expression, Statement] {
  def block: Seq[Statement]
  def resultingExpressions: Seq[Expression]

  /** Appends a code block that depends on the results of this code block */
  def appendDependent(
    other: Seq[Expression] => CodeBlockWithResultingExpressions[Expression, Statement]
  ): CodeBlockWithResultingExpressions[Expression, Statement] = {
    val nextBlock = other(resultingExpressions)
    val allStatements = block ++ nextBlock.block
    new CodeBlockWithResultingExpressions[Expression, Statement] {
      def block: Seq[Statement] =  allStatements
      def resultingExpressions: Seq[Expression] = nextBlock.resultingExpressions
    }
  }

  /** Appends an independent code block and concatenate its results */
  def appendIndependent(
    other: CodeBlockWithResultingExpressions[Expression, Statement]
  ): CodeBlockWithResultingExpressions[Expression, Statement] = {
    val allStatements = block ++ other.block
    val allResults = resultingExpressions ++ other.resultingExpressions
    new CodeBlockWithResultingExpressions[Expression, Statement] {
      def block: Seq[Statement] =  allStatements
      def resultingExpressions: Seq[Expression] = allResults
    }
  }
}

/** Provides Helpers to construct code blocks with result expressions */
object CodeBlockWithResultingExpressions {
  /** Constructs a new code block without any content or results. */
  def empty[Expression, Statement]: CodeBlockWithResultingExpressions[Expression, Statement] = apply()

  /** Constructs an empty code block with the given `resultExps` as results. */
  def apply[Expression, Statement](
    resultExps: Expression*
  ): CodeBlockWithResultingExpressions[Expression, Statement] =
    new CodeBlockWithResultingExpressions[Expression, Statement] {
      def block: Seq[Statement] = Seq.empty
      def resultingExpressions: Seq[Expression] = resultExps
    }

  /** Constructs a code block containing `stmts` with `resultExps` as results. */
  def apply[Expression, Statement]
    (stmts: Statement*)
    (resultExps: Expression*): CodeBlockWithResultingExpressions[Expression, Statement] =
    new CodeBlockWithResultingExpressions[Expression, Statement] {
      def block: Seq[Statement] = stmts
      def resultingExpressions: Seq[Expression] = resultExps
    }
}