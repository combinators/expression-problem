package example.expression.haskell    /*DI:LD:AI*/

import example.expression.domain.{BaseDomain, ModelDomain}

/**
  * Any Haskell-based approach capable of supporting Producer must provide this capability.
  */
trait StraightProducer extends Producer  {
  val domain:BaseDomain with ModelDomain

  /**
    * For producer operations, there is a need to instantiate objects, and one would use this
    * method (with specific parameters) to carry this out.
    */
  override def inst(exp:domain.Atomic)(op:domain.Operation)(params:Haskell*): Haskell = {
    Haskell("FILL IN PLEASE")
  }
}
