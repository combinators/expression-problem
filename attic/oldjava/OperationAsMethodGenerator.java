package org.combinators.ep.language.java;

/**
  * Some solutions have classes that are represented by a base class and then one class for
  * each of the known data types.
  * @deprecated
  */
trait OperationAsMethodGenerator {

  /**
    * Operations for a given Exp DataType are implemented as a method. */
  def methodGenerator(exp:DataType, op:Operation) : MethodDeclaration

}
