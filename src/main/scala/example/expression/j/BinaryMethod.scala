package example.expression.j  /*DI:LD:AI*/

import com.github.javaparser.ast.expr.Expression
import example.expression.domain.{BaseDomain, ModelDomain}
import org.combinators.templating.twirl.Java

/**
  * Any Java-based approach capable of supporting BinaryMethod Operations must provide this capability.
  */
trait BinaryMethod  {
  val domain:BaseDomain with ModelDomain

  /** Retrieve Java Class associated with given context. Needed for BinaryMethods operations with BaseTypeRep as parameter. */
  def getJavaClass : Expression
}
