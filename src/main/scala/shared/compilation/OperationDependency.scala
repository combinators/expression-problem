package shared.compilation

import expression.Operation

/**
  * If operation is dependent upon any other operations, maintain this information.
  *
  * Initially, no dependencies anywhere
  */
trait OperationDependency {

  /**
    * To declare a dependency, override this method and add to list
    * @param op
    * @return
    */
  def dependency(op:Operation) : List[Operation] = List.empty
}
