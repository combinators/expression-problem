package ep.cpp   /*DI:LD:AI*/

import ep.domain.ModelDomain
import org.combinators.ep.domain.{BaseDomain, ModelDomain}
import org.combinators.ep.generator.Producer

/**
  * Any C++-based approach capable of supporting Producer must provide this capability.
  */
trait CPPProducer extends Producer {
  val domain:BaseDomain with ModelDomain

  type InstanceExpression = CPPElement

  /**
    * For producer operations, there is a need to instantiate objects, and one would use this
    * method (with specific parameters) to carry this out.
    */
  def inst(exp:domain.Atomic, params:InstanceExpression*): InstanceExpression = {
    new CPPElement("new " + exp.concept + "(" + params.map(expr => expr.toString()).mkString(",") + ")")
  }
}
