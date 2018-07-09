package example.expression.domain

// + means something special in SCala
class WithDomain[+E <: BaseDomain](val domain:E) {
}

trait Evolution {

  /** Every evolution has associated domain. */
  val domain: ModelDomain

  def getModel: domain.Model
}
