package example.expression.haskell

/*DI:LD:AI*/

import example.expression.domain.{BaseDomain, ModelDomain}

/**
  * Any Java-based approach capable of supporting Producer must provide this capability.
  */
trait Producer  {
  val domain:BaseDomain with ModelDomain

  val flat:domain.Model
  def standardArgs(exp:domain.Atomic) : String
  /**
    * Expand instance into its post-order traversal of interior definitions.
    *
    * a1 = (7*2)+8
    *
    * a1LL = In(El(Constant 7.0)) :: GeneralExpr
    * a1LR = In(El(Constant 2.0)) :: GeneralExpr
    * a1R = In(El(Constant 8.0)) :: GeneralExpr
    * a1L = In(Er(Er(BinaryMul a1LL a1LR))) :: GeneralExpr
    * a1 = In(Er(El(BinaryPlus a1L a1R))) :: GeneralExpr

    */
  def treeRoute(a:domain.Atomic, flattened:Seq[domain.Atomic]) : String = {
    if (flattened.size == 1) {
      s"${a.name.capitalize}"
    } else if (a == flattened.head) {
      s"El(${a.name.capitalize} "
    } else {
      "Er(" + treeRoute(a, flattened.tail) + " "
    }
  }

  // ugly! works, though... COPIED from TestGenerator. Find better solution
  def closeTreeRoute(a:domain.Atomic, flattened:Seq[domain.Atomic]) : String = {
    if (flattened.size == 1) {
      ""
    } else if (a == flattened.head) {
      ")"
    } else {
      ")" + closeTreeRoute(a, flattened.tail)
    }
  }

  /**
    * For producer operations, there is a need to instantiate objects, and one would use this
    * method (with specific parameters) to carry this out.
    * In(Er(El(Op a1 a2)))
    * or
    * In(Er(El(Op 2.0 7.0)))
    */
  def inst(exp:domain.Atomic)(op:domain.Operation)(params:String*): Haskell = {
    Haskell("In(" + treeRoute(exp, flat.types) + " " + params.mkString(" ") + closeTreeRoute(exp, flat.types) +")")
  }
}
