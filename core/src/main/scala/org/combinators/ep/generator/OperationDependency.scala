package org.combinators.ep.generator

import org.combinators.ep.domain.abstractions.Operation

/** Models knowledge of operational dependencies arising when implementing code for some domain. */
trait OperationDependencies {
  /** Returns the operations the implementation of the given operation dependes on. */
  def dependencies(op:Operation) : Set[Operation] = Set.empty
}
