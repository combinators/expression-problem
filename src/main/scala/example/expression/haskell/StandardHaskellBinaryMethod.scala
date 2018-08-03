package example.expression.haskell     /*DI:LD:AI*/

import example.expression.domain.{BaseDomain, ModelDomain}
import example.expression.generator.BinaryMethod

/**
* Standard uses no context.
*/
trait StandardHaskellBinaryMethod extends BinaryMethod {
  val domain:BaseDomain with ModelDomain

  // only approach-dependent code knows how to handle binary methods
  override def binaryContext: String = ""
}
