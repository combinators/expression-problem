package org.combinators.ep.language.java     /*DI:LD:AI*/

import com.github.javaparser.ast.CompilationUnit

/**
  * Some solutions have classes that are represented by a base class and then one class for
  * each of the known data types.
  *
  * Deprecate this interface since it is causing more confusion than it eliminates. Specifically
  * when the model is passed in as a parameter, it means different things to different EP approaches.
  *
  * Currently implemented by Interpreter, OO, Visitor
  */
trait DataTypeSubclassGenerator {
  val domain:ModelDomain

  /** Generate the full class for the given expression sub-type. */
  def generateExp(model:domain.Model, e:domain.DataType) : CompilationUnit

  /** Generate the base class. */
  def generateBase(model:domain.Model) : CompilationUnit
}
