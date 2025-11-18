package org.combinators.model

abstract class Model {
  val dimensionality: Int
  val baseType: String

  // dp is always the storage for problem
  // solution is always the storage for solution
  val relation: Relation
}