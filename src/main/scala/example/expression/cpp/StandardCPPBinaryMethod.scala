package example.expression.cpp

import example.expression.domain.{BaseDomain, ModelDomain}
import example.expression.generator.BinaryMethod

trait StandardCPPBinaryMethod extends BinaryMethod {
  val domain:BaseDomain with ModelDomain

  // only approach-dependent code knows how to handle binary methods
  override def binaryContext: String = ""
}