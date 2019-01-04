package example.expression.scala   /*DI:LD:AI*/

import example.expression.domain.{BaseDomain, ModelDomain}
import scala.meta.Term

/**
  * Any Scala-based approach capable of supporting Producer must provide this capability.
  *
  * Note: Should have used localized types as defined within ScalaGenerator...
  */
trait Producer extends ScalaGenerator {
  val domain:BaseDomain with ModelDomain
  import domain._

  /**
    * For producer operations, there is a need to instantiate objects, and one would use this
    * method (with specific parameters) to carry this out.
    */
  def inst(exp:Atomic)(op:Operation)(params:Term*): Term = {
    Scala("new " + exp.name.capitalize + "(" + params.map(expr => expr.toString()).mkString(",") + ")").expression()
  }

}
