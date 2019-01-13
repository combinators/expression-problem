package example.expression.cpp

import example.expression.domain.{BaseDomain, ModelDomain}

/**
  * Any C++-based approach capable of supporting Producer must provide this capability.
  */
trait Producer  {
  val domain:BaseDomain with ModelDomain

  /**
    * For producer operations, there is a need to instantiate objects, and one would use this
    * method (with specific parameters) to carry this out.
    */
  def inst(exp:domain.Atomic)(op:domain.Operation)(params:CPPElement*): CPPElement = {
    new CPPElement("new " + exp.name.capitalize + "(" + params.map(expr => expr.toString()).mkString(",") + ")")
  }
}
