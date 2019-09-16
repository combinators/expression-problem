package org.combinators.ep.language.java;

trait ReplaceCovariantType {
  /** Inplace modification to replace a type in every covariant position. **/
  def replaceInCovariantPosition(original: Type, by: Type): Unit
}
