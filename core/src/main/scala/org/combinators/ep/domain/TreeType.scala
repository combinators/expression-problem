package org.combinators.ep.domain    /*DI:LI:AI*/

/** Special operation that declares underlying support for BinaryMethods. */
case object TreeType extends TypeRep {
  type scalaInstanceType = tree.Tree
}
