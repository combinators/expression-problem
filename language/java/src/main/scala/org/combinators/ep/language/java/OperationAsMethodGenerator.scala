package org.combinators.ep.language.java   /*DI:LD:AI*/

import com.github.javaparser.ast.body.MethodDeclaration
import org.combinators.ep.domain.ModelDomain

/**
  * Some solutions have classes that are represented by a base class and then one class for
  * each of the known data types.
  * @deprecated
  */
trait OperationAsMethodGenerator {
  val domain:ModelDomain

  /**
    * Operations for a given Exp DataType are implemented as a method. */
  def methodGenerator(exp:domain.DataType, op:domain.Operation) : MethodDeclaration

}
