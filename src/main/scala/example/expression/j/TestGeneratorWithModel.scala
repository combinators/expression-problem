package example.expression.j

import example.expression.domain.{BaseDomain, ModelDomain}

/**
  * Some test cases demand having access to the model
  */
trait TestGeneratorWithModel extends TestGenerator  {
  val domain:BaseDomain with ModelDomain

  def getModel:domain.Model
}
