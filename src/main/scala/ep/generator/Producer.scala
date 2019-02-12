package ep.generator     /*DI:LI:AI*/

import ep.domain.{BaseDomain, ModelDomain}

/**
  * Any generator that works with Producer methods must incorporate this trait.
  */
trait Producer {
  val domain:BaseDomain with ModelDomain
//
//  type InstanceExpression
//
//  /**
//    * For producer operations, there is a need to instantiate objects, and one would use this
//    * method (with specific parameters) to carry this out.
//    */
//  def inst(exp:domain.Atomic, params:Expression*): Expression
}
