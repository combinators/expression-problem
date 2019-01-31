package ep.j  /*DI:LD:AI*/

import ep.domain.{BaseDomain, ModelDomain}
import ep.generator.Producer
import org.combinators.templating.twirl.Java

/**
  * Any Java-based approach capable of supporting Producer must provide this capability.
  */
trait JavaProducer extends Producer {
//  val domain:BaseDomain with ModelDomain
//
//  type InstanceExpression = com.github.javaparser.ast.expr.Expression
//
//  /**
//    * For producer operations, there is a need to instantiate objects, and one would use this
//    * method (with specific parameters) to carry this out.
//    */
//  def inst(exp:domain.Atomic, params:InstanceExpression*): InstanceExpression = {
//    Java("new " + exp.name.capitalize + "(" + params.map(expr => expr.toString).mkString(",") + ")").expression()
//  }
}
