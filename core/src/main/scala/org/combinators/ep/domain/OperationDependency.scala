package org.combinators.ep.domain    /*DI:LI:AI*/

/**
  * If operation is dependent upon any other operations, maintain this information.
  *
  * Initially, no dependencies anywhere
  */
trait OperationDependency {
  val domain:BaseDomain with ModelDomain

  /**
    * To declare a dependency, override this method and add to list
    * @param op
    * @return
    */
  def dependency(op:domain.Operation) : List[domain.Operation] = List.empty
}
