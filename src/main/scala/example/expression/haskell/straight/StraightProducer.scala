package example.expression.haskell.straight

/*DI:LD:AI*/

import example.expression.domain.{BaseDomain, ModelDomain}
import example.expression.haskell.{Haskell, HaskellProducer}

/**
  * Any Haskell-based approach capable of supporting Producer must provide this capability.
  */
trait StraightProducer extends HaskellProducer  {
//  val domain:BaseDomain with ModelDomain
//
//  /**
//    * For producer operations, there is a need to instantiate objects, and one would use this
//    * method (with specific parameters) to carry this out.
//    */
//  override def inst(exp:domain.Atomic, params:Haskell*): Haskell = {
//    Haskell(exp.name.capitalize + " " + params.map(h => h.getCode).mkString(" "))
//  }
}
