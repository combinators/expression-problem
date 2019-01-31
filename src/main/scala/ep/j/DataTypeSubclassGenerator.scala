package ep.j  /*DI:LD:AI*/

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.expr.Expression
import ep.domain.ModelDomain
import org.combinators.templating.twirl.Java

/**
  * Some solutions have classes that are represented by a base class and then one class for
  * each of the known data types.
  */
trait DataTypeSubclassGenerator {
  val domain:ModelDomain

  /** Generate the full class for the given expression sub-type. */
  def generateExp(model:domain.Model, e:domain.Atomic) : CompilationUnit

  /** Generate the base class. */
  def generateBase(model:domain.Model) : CompilationUnit

}
