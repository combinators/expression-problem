package example.expression.j

import com.github.javaparser.ast.expr.Expression
import example.expression.domain.{BaseDomain, ModelDomain}
import org.combinators.templating.twirl.Java

/**
  * Any Java-based approach capable of supporting Producer must provide this capability.
  */
trait Producer  {
  val domain:BaseDomain with ModelDomain

  /**
    * For producer operations, there is a need to instantiate objects, and one would use this
    * method (with specific parameters) to carry this out.
    *
    */
  def inst(exp:domain.Atomic)(op:domain.Operation)(params:Expression*): Expression = {
    Java("new " + exp.name.capitalize + "(" + params.map(expr => expr.toString()).mkString(",") + ")").expression()
  }

}
