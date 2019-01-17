package example.expression.j  /*DI:LD:AI*/

import com.github.javaparser.ast.body.MethodDeclaration
import example.expression.domain.ModelDomain

/**
  * Some solutions have classes that are represented by a base class and then one class for
  * each of the known data types.
  */
trait OperationAsMethodGenerator {
  val domain:ModelDomain

  /**
    * Operations for a given Exp DataType are implemented as a method. */
  /** Should pass in context, which is either 'e' or ''.'' */
  def methodGenerator(exp:domain.Atomic, op:domain.Operation) : MethodDeclaration

}
