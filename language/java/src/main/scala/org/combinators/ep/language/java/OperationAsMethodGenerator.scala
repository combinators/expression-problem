package ep.j  /*DI:LD:AI*/

import com.github.javaparser.ast.body.MethodDeclaration
import org.combinators.ep.domain.ModelDomain

/**
  * Some solutions have classes that are represented by a base class and then one class for
  * each of the known data types.
  */
trait OperationAsMethodGenerator {
  val domain:ModelDomain

  /**
    * Operations for a given Exp DataType are implemented as a method. */
  def methodGenerator(exp:domain.Atomic, op:domain.Operation) : MethodDeclaration

}
