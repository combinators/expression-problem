package example.expression.haskell    /*DI:LD:AI*/

import example.expression.domain.{BaseDomain, ModelDomain}
import example.expression.generator.Producer

/**
  * Any Haskell-based approach capable of supporting Producer must provide this capability.
  */
trait HaskellProducer extends Producer {
//  val domain:BaseDomain with ModelDomain
//
//  type InstanceExpression = Haskell
//
//  /**
//    * For producer operations, there is a need to instantiate objects, and one would use this
//    * method (with specific parameters) to carry this out.
//    */
//  def inst(exp:domain.Atomic, params:InstanceExpression*): InstanceExpression
}
