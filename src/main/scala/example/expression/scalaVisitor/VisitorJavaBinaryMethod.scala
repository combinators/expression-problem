package example.expression.scalaVisitor    /*DI:LD:AD*/

import example.expression.domain.{BaseDomain, ModelDomain}
import example.expression.generator.BinaryMethod

/**
  * Visitor uses context of "e." to access properly
  *
  *    return e.astree().same(that.astree());
  *
  */
trait VisitorJavaBinaryMethod extends BinaryMethod {
  val domain:BaseDomain with ModelDomain

  // only approach-dependent code knows how to handle binary methods
  override def binaryContext: String = "e."
}
