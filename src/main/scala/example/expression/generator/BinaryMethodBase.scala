package example.expression.generator    /*DI:LD:AI*/

import example.expression.domain.{BaseDomain, ModelDomain}

/**
  * Must be part of an evolution prior to the inclusion of any Binary Methods.
  *
  * This is used, for example, within scalaVisitor to properly deal with context in
  * AsTree operation.
  */
trait BinaryMethodBase  {
  val domain:BaseDomain with ModelDomain

  // Top-level declarations may need to be defined to augment operations
  type Declaration

  /**
    * Top-level constructs to be added as needed.
    */
  def declarations: Seq[Declaration]
}
