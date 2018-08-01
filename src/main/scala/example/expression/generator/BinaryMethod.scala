package example.expression.generator    /*DI:LD:AI*/

import example.expression.domain.{BaseDomain, ModelDomain}

/**
  * Any operation supporting BinaryMethod Operations must provide this capability.
  *
  * Any BinaryMethod operation has access to these declarations and can use accordingly.
  */
trait BinaryMethod  {
  val domain:BaseDomain with ModelDomain

  /**
    * In certain binary methods, an operation needs a context within which to run, something
    * that is "expression." or "".
    */
  def binaryContext: String
}
