package example.expression.generator    /*DI:LD:AI*/

import example.expression.domain.{BaseDomain, ModelDomain}

/**
  * Any operation supporting BinaryMethod Operations must provide this capability.
  *
  * Any BinaryMethod operation has access to these declarations and can use accordingly.
  */
trait BinaryMethod  {
  val domain:BaseDomain with ModelDomain

  /** abstract. */
  type expt

  /**
    * Provide default implementation that works for most cases.
    *
    * If you want to have "expression." before, then override and update as well.  and could use 'Expression' instead of
    * 'String' for further type safety.
    */
  def inBinaryContext(e:expt) : expt = e

  /**
    * In certain binary methods, an operation needs a context within which to run, something
    * that is "expression." or "".
    */
  def binaryContext: String
}
